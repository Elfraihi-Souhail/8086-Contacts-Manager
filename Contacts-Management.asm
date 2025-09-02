; ============================================================================================
; CONTACT MANAGEMENT SYSTEM
; This program implements a simple contact management system for DOS
; Allows adding, viewing, searching, modifying and deleting contacts
; Each contact consists of a name (10 chars max) and phone number (10 digits)
; ============================================================================================

data segment
    ; Data definitions
    buffer db 11, ?, 11 dup('$')  ; Buffer for keyboard input (max 10 chars + CR)
                                  ; Format: max size, actual size, characters + '$' terminator
    array db 352 dup(?)           ; Destination array for contact storage:
                                  ; 320 bytes for data (16 contacts × 20 bytes each) + 32 bytes for terminators
    choice db 2, ?, 2 dup('$')    ; Buffer for menu choice input
    count db 0                    ; Counter to track number of contacts in system (max 16)
    
                  
    ;MESSAGES - String constants used throughout the program
    menu db "----------------MENU----------------$"
    first db "1.ADD A CONTACT (NAME + PHONE)$"
    second db "2.VIEW ALL CONTACTS$"
    third db "3.SEARCH A CONTACT$"
    fourth db "4.MODIFY A CONTACT$"
    fifth db "5.DELETE A CONTACT$"
    sixth db "6.DISPLAY NAMES WITH SAME PREFIX$"
    seventh db "7.DISPLAY PHONE NUMS WITH THE SAME SEQUENCE$"
    eith db "8.EXIT$"
    underline db "------------------------------------$" 
    line db '~~~~~~~~~$'
    prompt db 'Enter a choice : $'
    msg1 db 'Enter the name (10 chars max) :$'
    msg2 db 'Enter the phone number (10 digits) :$'
    msg3 db 'CONTACTS :$'
    msg4 db 'NAME : $'
    msg5 db 'PHONE : $'
    msg6 db 'Enter the name you looking for :$'
    msg7 db 'Contact found , the pfone number is : $'
    msg8 db 'Enter the new phone number :$'
    msg9 db 'Contact have been deleted.$'   
    msg10 db 'Enter the prefix :$'
    msg11 db 'Enter the sequence :$'
    
    ;ERROR STATUS MESSAGES
    error1 db 'Invalid choice!$'
    error2 db 'The contacts are full (16/16)$'
    error3 db 'The contacts list is empty.$'
    error4 db 'No contact found.$'
    error5 db 'Invalid phone number (must contain 10 digits)!$'
    error6 db 'Invalid phone number (must contain digits from 0-9)!$'
    error7 db 'Prefix not found$'
    error8 db 'Sequence not found$'
    
    new_line db 13,10,'$'         ; Carriage return + line feed sequence for new lines
    pkey db "Press any key...$"   ; Exit prompt
        
ends

stack segment
    dw 128 dup(0)                 ; 128-word stack (256 bytes)
ends

code segment
start:
    ; Initialize segment registers to point to our data segment
    mov ax, data
    mov ds, ax
    mov es, ax
    
    ; ============================================================================================
    ; MACRO DEFINITIONS
    ; ============================================================================================
        
    ; Macro to add a string terminator ('$') at the end of user input buffer
    terminator macro buff
        push bx                   ; Save registers
        push cx

        lea bx, buff + 1          ; Load effective address of the length byte
        mov cl, [bx]              ; Get the length value
        mov ch, 00h               ; Clear upper part of CX
        add bx, cx                ; Add length to get to the last character
        mov byte ptr [bx+1], '$'  ; Place string terminator after last character
    
        pop cx                    ; Restore registers
        pop bx      
    endm
        
    ; Macro to read a string from keyboard into a buffer
    sscanf macro buff
        push ax                   ; Save registers
        push dx
        mov ah, 0Ah               ; DOS function: Buffered input
        lea dx, buff              ; Load address of the buffer
        int 21h                   ; Call DOS interrupt 
        terminator buff
        pop dx                    ; Restore registers
        pop ax
    endm 
    
    ; Macro to validate phone number input
    ; Checks if input is exactly 10 digits and all chars are numeric (0-9)
    check macro buff,bx
        push ax
        push cx
        push si
        
        mov al,[buff + 1]         ; Get actual length of input
        cmp al,10                 ; Check if exactly 10 digits
        jne invalid1              ; Jump if not 10 digits
        mov ah,00h
        lea si,buff + 2           ; Point to start of input string
        mov cx,0010               ; Loop counter - check all 10 digits
        check_loop: mov al,[si]   ; Load current character
                    cmp ax,'0'    ; Check if less than '0'
                    jl invalid2   ; If yes, not a digit
                    mov al,[si]
                    cmp al,'9'    ; Check if greater than '9'
                    jg invalid2   ; If yes, not a digit
                    inc si        ; Move to next character
        loop check_loop           ; Continue until all chars checked
        mov bx,0000h              ; Set return code to 0 (valid)
        jmp end_check
                    
        invalid1: println error5  ; Display "must be 10 digits" error
                  mov bx,0001h    ; Set return code to 1 (invalid)
        jmp end_check
        invalid2: println error6  ; Display "must contain only digits" error
                  mov bx,00001h   ; Set return code to 1 (invalid)
        
        end_check: pop si         ; Restore registers
                   pop cx
                   pop ax               
    endm
    
    ; Macro to copy string from input buffer to destination in contact array
    strcpy macro buff,text 
        push si                   ; Save registers
        push di
        push cx
        cld                       ; Clear direction flag (increment addresses)
        lea si, buff + 2          ; Source address (skip buffer metadata)
        lea di, text              ; Destination address
        mov cx, 11                ; Copy 11 bytes (10 chars + terminator)
        rep movsb                 ; Repeat: Move byte from DS:SI to ES:DI and increment
        pop cx                    ; Restore registers
        pop di
        pop si              
    endm
    
    ; Macro to swap two contact entries (used in sorting)
    ; Each contact is 22 bytes (11 for name, 11 for phone)
    swap macro x,y
        push si                   ; Save registers
        push di
        push cx
        push bx
        push ax
        mov si,x                  ; Source contact address
        mov di,y                  ; Destination contact address
        mov cx,22                 ; Size of contact record
    swap_loop:                    ; Byte-by-byte swap
        mov al,[si]               ; Get byte from source
        mov bl,[di]               ; Get byte from destination
    
        mov [si],bl               ; Place destination byte in source
        mov [di],al               ; Place source byte in destination
        
        inc si                    ; Move to next byte
        inc di
                    
    loop swap_loop                ; Continue until all bytes swapped
        pop ax                    ; Restore registers
        pop bx
        pop cx
        pop di
        pop si              
    endm
    
    ; Macro to clear and reset an input buffer
    clear_buffer macro buff
        push cx                   ; Save registers
        push di
        push ax

        mov  [buff], 11           ; Set max length
        mov  [buff+1], 0          ; Set current length to 0
    
        ; Fill buffer with terminators
        lea di, [buff+2]          ; Start of buffer content
        mov cx, 11                ; 11 bytes to fill
        mov al, '$'               ; Fill with terminators
        rep stosb                 ; Repeat: Store AL at ES:DI and increment DI
    
        pop ax                    ; Restore registers
        pop di
        pop cx
    endm
    
    ; Macro to display a string
    printf macro text
        push ax                   ; Save registers
        push dx
        lea dx, text              ; Load address of string
        mov ah, 09h               ; DOS function: Display string
        int 21h                   ; Call DOS interrupt
        pop dx                    ; Restore registers
        pop ax 
    endm
    
    ; Macro to display a string with a line break first
    println macro text
        call next_line            ; Insert a line break
        push ax                   ; Save registers
        push dx
        lea dx, text              ; Load address of string
        mov ah, 09h               ; DOS function: Display string
        int 21h                   ; Call DOS interrupt
        pop dx                    ; Restore registers
        pop ax 
    endm
    
    ; ============================================================================================
    ; MAIN PROGRAM FLOW
    ; ============================================================================================
    
    main: 
         call display_menu        ; Show menu options
         sscanf  choice           ; Get user's menu selection
         
         ; Check which menu option was selected
         cmp [choice + 2],'1'     ; Compare to '1' (Add contact)
         je  choice1
         cmp [choice + 2],'2'     ; Compare to '2' (View contacts)
         je  choice2
         cmp [choice + 2],'3'     ; Compare to '3' (Search contact)
         je  choice3
         cmp [choice + 2],'4'     ; Compare to '4' (Modify contact)
         je  choice4
         cmp [choice + 2],'5'     ; Compare to '5' (Delete contact)
         je  choice5
         cmp [choice + 2],'6'     ; Compare to '6' (Display names whith same prefix)
         je choice6 
         cmp [choice + 2],'7'
         je choice7
         cmp [choice + 2],'8'     ; Compare to '6' (Exit)
         je exit
         
         println error1           ; If none matched, display invalid choice error
         
         jmp main                 ; Return to menu
         
    choice1: call add_contact     ; Handle add contact option
             jmp main             ; Return to menu
    
    choice2: call view_contacts   ; Handle view contacts option
             jmp main             ; Return to menu
    
    choice3: call search          ; Handle search contact option
             jmp main             ; Return to menu
    
    choice4: call modify          ; Handle modify contact option
             jmp main             ; Return to menu
    
    choice5: call delete          ; Handle delete contact option
             jmp main             ; Return to menu 
             
    choice6: call name_prefix     ; Display names whith prefix
             jmp main
             
    choice7: call phone_seq
             jmp main
    
    ; Exit sequence
    exit:   println pkey          ; Display exit message
            ; Wait for key press
            mov ah, 01h           ; DOS function: Read character
            int 21h               ; Call DOS interrupt
    
            ; Exit to DOS
            mov ax, 4c00h         ; DOS function: Exit with return code 0
            int 21h               ; Call DOS interrupt

    ; ============================================================================================
    ; PROCEDURES
    ; ============================================================================================
    
    ; Clear screen procedure
    proc clrscr
        push ax                   ; Save registers
        push cx
          
        mov cx, 000bh             ; Delay time (high word)
        mov dx, 73ah              ; Delay time (low word)
        mov ah, 86h               ; BIOS function: Wait
        int 15h                   ; Call BIOS interrupt
        mov ah, 0h                ; BIOS function: Set video mode
        mov al, 3h                ; 80x25 text mode (clears screen)
        int 10h                   ; Call video interrupt
         
        pop cx                    ; Restore registers
        pop ax   
    ret
    
    ; Insert new line procedure
    proc next_line   
        push dx                   ; Save registers
        push ax
        lea dx, new_line          ; Load address of new line string
        mov ah,09h                ; DOS function: Display string
        int 21h                   ; Call DOS interrupt
        pop ax                    ; Restore registers
        pop dx
    ret
    
    ; Display program menu procedure
    proc display_menu
        call clrscr               ; Clear screen
        println menu              ; Display menu header
        println first             ; Display option 1
        println second            ; Display option 2
        println third             ; Display option 3
        println fourth            ; Display option 4
        println fifth             ; Display option 5
        println sixth             ; Display option 6
        println seventh           ; Display option 7
        println eith              ; Display option 8
        println underline         ; Display separator
        println prompt            ; Display input prompt
    ret
    
    ; Add contact procedure
    proc add_contact 
        call clrscr               ; Clear screen
        push bx                   ; Save registers
        push cx
        push di
        push si
        push ax
  
        mov al, [count]           ; Get current contact count
        cmp al, 16                ; Check if contact list is full (16/16)
        jge full                  ; If full, display error
        
        ; Get contact name input
        println msg1              ; Prompt for name
        sscanf buffer             ; Get name input
        terminator buffer         ; Add string terminator to input
        
        ; Calculate position in array for new contact
        mov bl,22                 ; Each contact entry is 22 bytes
        mul bl                    ; Multiply by count to get offset
        lea di,array              ; Load base address of array
        add di,ax                 ; Add offset to get position
        
        strcpy buffer,di          ; Copy name to array
        clear_buffer buffer       ; Clear input buffer
    
        add di,11                 ; Move to phone number position
        
        ; Get and validate phone number input
        phone_input:              ; Label for retry on invalid input
        println msg2              ; Prompt for phone number
        sscanf buffer             ; Get phone number input
        terminator buffer         ; Add string terminator
        push bx                   ; Save BX
        mov bx,0000h              ; Initialize return code
        check buffer,bx           ; Validate phone number
        cmp bx,0001h              ; Check if invalid
        je phone_input            ; If invalid, retry
        pop bx                    ; Restore BX
        strcpy buffer,di          ; Copy phone number to array
        clear_buffer buffer       ; Clear input buffer
        inc count                 ; Increment contact count
        
        jmp done                  ; Skip error message
    
        full: println error2      ; Display "contacts full" error
        
        done:                     ; Clean up and return
        pop ax                    ; Restore registers
        pop si
        pop di
        pop cx
        pop bx
    ret
    
    ; Sort contacts using selection sort algorithm (by name)
    proc selective_sort 
        pusha                     ; Save all registers
        
        lea si,array              ; SI points to first contact
        mov al,[count]            ; Get number of contacts
        cmp al,1                  ; If 0 or 1 contact, no need to sort
        jle sorted
        dec al                    ; Set outer loop counter to n-1
        mov cl,al
        mov ch,00h                ; Clear high byte of CX
        
        ; Outer loop - for each position except last
        outside_loop: mov di,si   ; DI = assumed minimum element index
                      push si     ; Save outer loop position
                      
                      mov bx,si   ; BX = current comparison position
                      push cx     ; Save outer loop counter
                      
                      ; Inner loop - find minimum element
                      inside_loop: add bx,22            ; Move to next contact
                                   push bx              ; Save registers
                                   push si
                                   push di
                                   push cx              ; Save inner loop counter
                                   
                                   ; Compare names (min vs current)
                                   mov cl,11            ; Compare 11 bytes
                                   mov si,di            ; SI = min name address
                                   mov di,bx            ; DI = current name address
                                   rep cmpsb            ; Compare strings
                                   
                                   ; Restore registers
                                   pop cx
                                   pop di
                                   pop si
                                   pop bx
                                   jle not_min          ; If min <= current, skip update
                                   
                                   ; Update minimum if current is smaller
                                   min: mov di,bx       ; New minimum found
                                   not_min: dec cx      ; Decrement inner counter
                      jnz inside_loop                   ; Continue inner loop if not done
                      pop cx                            ; Restore outer loop counter
                      
                      ; Swap if needed
                      cmp di,si                         ; Check if minimum changed
                      je no_swap                        ; If not, skip swap
                      
                      swap si,di                        ; Swap contacts
                      
                      no_swap: pop si                   ; Restore outer loop position
                               add si,22                ; Move to next position
                               dec cx                   ; Decrement outer counter
        jnz outside_loop                                ; Continue outer loop if not done
                
    sorted: popa                  ; Restore all registers
    ret
    
    ; Display all contacts
    proc view_contacts
        call selective_sort       ; Sort contacts first
        call clrscr               ; Clear screen
        push ax                   ; Save registers
        push di
        push bx
        push cx
        
        mov cl,[count]            ; Get contact count
        cmp cl,0h                 ; Check if empty
        je empty                  ; If empty, display error
        println msg3              ; Display "CONTACTS:" heading
        println line              ; Display separator
        mov ch,0h                 ; Clear high byte of CX
        lea di,array              ; DI points to first contact
        mov bl,22                 ; Each contact is 22 bytes
        
        ; Loop through all contacts
        for:
        println msg4              ; Display "NAME:" label
        printf di                 ; Display contact name
        add di,11                 ; Move to phone number
        println msg5              ; Display "PHONE:" label
        printf di                 ; Display phone number
        println underline         ; Display separator
        add di,11                 ; Move to next contact
        loop for                  ; Continue until all contacts displayed

        jmp not_empty             ; Skip error message
    
        empty: println error3     ; Display "contacts empty" error
        
        not_empty: pop cx         ; Restore registers
                   pop bx
                   pop di
                   pop ax 
    ret
;------------------------------------------------------------------
    proc name_prefix
        pusha
        call clrscr
        
        mov cl,[count]
        cmp cl,00h
        je pre_notfound
        ;Read the prefix
        println msg10
        clear_buffer buffer
        sscanf buffer
        terminator buffer
        ;Loop on the array to search for the start of the string 
        mov bx,0000h
        mov ax,01h 
        mov ch,00h
        mov cl,[count] 
        
        prename_loop: push cx 
                    ;searching the same start in the array
                    lea di,buffer + 2
                    lea si,array + bx
                    mov ch,00h
                    mov cl,[buffer + 1]
                    push si
                    rep cmpsb
                    pop di
                    jne repeat
                    
                    ;print the name if found 
                    mov ax,00h
                    println msg4                            
                    printf di 
                    println msg5
                    add di,11
                    printf di
                    println underline
                     
                    
                    
                    repeat:pop cx
                           add bx,22                     
        loop prename_loop        
        ; AX works as a boolean to check if i printed a name or not            
        cmp ax,1
        jne end_prename
        
        pre_notfound: println error7
        
        end_prename: popa
    ret                          
;------------------------------------------------------------------------------                                     
    proc phone_seq
        pusha
        
        call clrscr
        
        mov cl,[count]
        cmp cl,00h
        je seq_notfound 
        
        ;Read the sequence
        println msg11
        clear_buffer buffer
        sscanf buffer
        terminator buffer
        ;Loop on the array to search for the sequence of the string 
        mov bx,11
        mov ax,01h 
        mov ch,00h
        mov cl,[count]
        
        seq_loop:      push cx
                       mov cx,11
                       lea si,array + bx
                       push bx
                                        ; Saving the values of the counter and the phone start
                       search_sequence: push cx 
                                        mov bx,0000h
                                        ; Compare the first digit of the number and the sequence
                                        add si,bx
                                        lea di,buffer + 2
                                        mov ax,[si]
                                        mov dx,[di]
                                        cmp ax,dx 
                                        pop cx                                       
                                        jne repeat_search
                                        ; If the first one is equal then we compare the rest of the sequence 
                                        push cx
                                        push si
                                        push di  
                                        
                                        mov cl,[buffer + 1]
                                        rep cmpsb 
                                        pop di
                                        pop si
                                        pop cx
                          
                                        ; If we found the sequence then we print the number
                                        je seq_found
                                        ; else we repeat until we complete the 10 digits
                                        repeat_search: inc bx
                                                      
                       loop search_sequence                  
                       
                       pop bx                
                       jmp seq_repeat
                                  ; We print the contact    
                       seq_found: pop bx
                                  lea di,array + bx
                                  sub di,11
                                  println msg4
                                  printf di
                                  lea di,array + bx
                                  println msg5
                                  printf di
                                  println underline 
                                  mov ax,00h
                                  
                       seq_repeat: add bx,11 
                                   pop cx
        loop seq_loop                            
        
        cmp ax,1
        jne end_seq
        
        seq_notfound: println error8
        
        end_seq: popa                                    
    
    ret 
;-----------------------------------------------------------------------
    ; Search for a contact by name
    proc search
        call clrscr               ; Clear screen
        pusha                     ; Save all registers
        
        mov al,[count]            ; Get contact count
        cmp al,0                  ; Check if empty
        je not_found              ; If empty, show not found error
    
        println msg6              ; Prompt for name to search
        sscanf buffer             ; Get search input
        terminator buffer         ; Add string terminator
    
        mov ch,00h                ; Clear high byte of CX
        mov cl,al                 ; Set loop counter to contact count
        lea si,array              ; SI points to first contact
        lea di,buffer + 2         ; DI points to search name
        cld                       ; Clear direction flag (increment)
        
        ; Loop through contacts to find match
        search_loop: 
                 push si          ; Save registers
                 push di
                 push cx
                 mov cx,10        ; Compare first 10 bytes (name length)
                 rep cmpsb        ; Compare strings
                 pop cx           ; Restore registers
                 pop di
                 pop si
                 
                 je found         ; If match found, display it
                 
                 add si,22        ; Move to next contact
                 dec cx           ; Decrement counter
        jnz search_loop           ; Continue if more contacts to check
        
        ; No match found
        not_found: println error4 ; Display "not found" error
                   jmp done2      ; Skip to cleanup
                
        ; Match found, display phone number
        found: add si,11          ; Move to phone number
               println msg7       ; Display "found" message
               printf si          ; Display phone number
                      
        done2: clear_buffer buffer ; Clear input buffer
               popa                ; Restore all registers
        
    ret
    
    ; Modify a contact's phone number
    proc modify 
        pusha                     ; Save all registers
        
        call clrscr               ; Clear screen
        mov al,[count]            ; Get contact count
        cmp al,0                  ; Check if empty
        je no_change              ; If empty, show error
        
        println msg6              ; Prompt for name to modify
        sscanf buffer             ; Get search input
        terminator buffer         ; Add string terminator
        
        cld                       ; Clear direction flag (increment)
        lea si,array              ; SI points to first contact
        lea di,buffer + 2         ; DI points to search name
        mov ch,00h                ; Clear high byte of CX
        mov cl,[count]            ; Set loop counter to contact count
        
        ; Loop through contacts to find match
        modify_loop: push si      ; Save registers
                     push di
                     push cx
                     mov cx,10    ; Compare first 10 bytes (name length)
                     rep cmpsb    ; Compare strings
                     
                     pop cx       ; Restore registers
                     pop di
                     pop si
                    
                     je change    ; If match found, get new phone number
                     add si,22    ; Move to next contact
        loop modify_loop          ; Continue if more contacts to check
        
        ; No match found
        no_change: println error4 ; Display "not found" error
                   jmp end_modify ; Skip to cleanup
        
        ; Match found, get new phone number
        ; Embedded validation similar to check macro
        change: clear_buffer buffer         ; Clear input buffer
                println msg8               ; Prompt for new phone number
                sscanf buffer              ; Get new phone number
                terminator buffer          ; Add string terminator
                mov al,[buffer + 1]        ; Get input length
                cmp al,10                  ; Check if exactly 10 digits
                jne invalid_in1            ; If not, show error
                mov ah,00h
                lea di,buffer + 2          ; DI points to input
                mov cx,0010                ; Set counter to 10 (digits)
                
        ; Validate each character is a digit
        check_input: mov al,[di]           ; Get current character
                     cmp ax,'0'            ; Check if less than '0'
                     jl invalid_in2        ; If yes, not a digit
                     mov al,[di]
                     cmp al,'9'            ; Check if greater than '9'
                     jg invalid_in2        ; If yes, not a digit
                     inc di                ; Move to next character
        loop check_input                   ; Continue until all checked
        jmp valid_in                       ; If all valid, update contact
                    
        invalid_in1: println error5        ; Display "must be 10 digits" error
                     jmp change            ; Try again
        invalid_in2: println error6        ; Display "must be digits" error
                     jmp change            ; Try again
        
        ; Update contact with new phone number
        valid_in:  add si,11               ; Move to phone number position
                   mov di,si               ; Set destination
                   strcpy buffer,di        ; Copy new number to contact
         
        end_modify:  clear_buffer buffer   ; Clear input buffer
                     popa                  ; Restore all registers
    ret
    
    ; Delete a contact
    proc delete 
        pusha                     ; Save all registers
        
        call clrscr               ; Clear screen
        mov al,[count]            ; Get contact count
        cmp al,0                  ; Check if empty
        je no_delete              ; If empty, show error
        
        println msg6              ; Prompt for name to delete
        sscanf buffer             ; Get search input
        terminator buffer         ; Add string terminator
        
        cld                       ; Clear direction flag (increment)
        lea si,array              ; SI points to first contact
        lea di,buffer + 2         ; DI points to search name
        mov ch,00h                ; Clear high byte of CX
        mov cl,[count]            ; Set loop counter to contact count
        
        ; Loop through contacts to find match
        delete_loop: push si      ; Save registers
                     push di
                     push cx
                     mov cx,10    ; Compare first 10 bytes (name length)
                     rep cmpsb    ; Compare strings
                     
                     pop cx       ; Restore registers
                     pop di
                     pop si
                    
                     je delete_element ; If match found, delete contact
                     add si,22    ; Move to next contact
        loop delete_loop          ; Continue if more contacts to check
        
        ; No match found
        no_delete: println error4 ; Display "not found" error
                   jmp end_delete ; Skip to cleanup
        
        ; Found match, mark for deletion by filling with 'z' characters
        delete_element: clear_buffer buffer ; Clear input buffer
                
                mov di,si         ; Set destination to contact name
                mov cx,10         ; 10 characters to fill
                mov al,'z'        ; Fill with 'z' characters
                rep stosb         ; Fill name with 'z'
 
                call selective_sort ; Sort contacts (moves 'z' to end)
                
                ; Decrement contact count
                mov al,[count]
                dec al
                mov count,al
                
                ; Clear the last contact slot (now redundant)
                mov ah,00h
                mov bx,22
                imul bx           ; Calculate offset to last contact
                
                mov cx,22         ; 22 bytes to clear
                lea di,array      ; Base address
                add di,ax         ; Add offset
                mov al,'$'        ; Fill with terminators
                rep stosb         ; Clear last contact
                
                println msg9      ; Display "deleted" message
                            
        end_delete: popa          ; Restore all registers
    ret       
                                
ends

end start