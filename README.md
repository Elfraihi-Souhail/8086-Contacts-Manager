# Contact Management System (8086 Assembly) 📞

A complete address book application written in 8086 Assembly Language, developed as a practical homework assignment for the **École Nationale Supérieure d'Informatique (ESI)**. This program runs in a DOS environment and allows full management of contacts with a user-friendly menu interface.

## 📋 Project Overview

This program implements a functional contact management system with the following features:
- **Add Contact**: Store a new contact (name + 10-digit phone number)
- **View All**: Display all stored contacts in sorted order
- **Search**: Find a contact by name
- **Modify**: Update a contact's phone number
- **Delete**: Remove a contact from the address book
- **Advanced Search**: Find names by prefix and phone numbers by digit sequence

## 🎓 Academic Context

**Practical Work Submission** for the **SYST2** course (2024-2025) at the **École Nationale Supérieure d'Informatique (ESI)**.

This project demonstrates foundational concepts in low-level programming and computer architecture as part of the preparatory class curriculum.

## 🛠️ Technical Specifications

- **Language**: x86 Assembly (8086)
- **Environment**: DOS / emu8086
- **Data Structure**: Array-based storage
- **Capacity**: 16 contacts maximum
- **Contact Format**: 
  - Name: 10 characters maximum
  - Phone: Exactly 10 digits (0-9)

## 🚀 How to Run (Using emu8086)

1.  **Download and Install** the [emu8086 application](https://emu8086.en.softonic.com/).
2.  **Open emu8086** and load the `Contacts-Management.asm` file.
3.  Click the **"Emulate"** button (play icon) to start the emulator.
4.  Alternatively, click **"Compile"** to create an executable `.exe` file, then run it in a DOS environment (like DOSBox).

## 🧩 Code Structure

The program is organized with clear macros and procedures:

### Key Macros:
- `sscanf`: Reads buffered keyboard input
- `printf`/`println`: Displays strings with formatting
- `strcpy`: Copies strings between memory locations
- `check`: Validates phone number input
- `swap`: Swaps two contact entries (for sorting)
- `clear_buffer`: Prepares input buffer for reuse

### Main Procedures:
- `add_contact`: Handles new contact creation with validation
- `view_contacts`: Displays sorted contacts (uses `selective_sort`)
- `search`: Finds a contact by name
- `modify`: Updates a contact's phone number
- `delete`: Removes a contact and maintains array integrity
- `name_prefix`: Finds contacts whose names start with a given prefix
- `phone_seq`: Finds contacts whose phone numbers contain a specific digit sequence

## 💾 Memory Management

- **Buffer Allocation**: Intelligent use of DOS function `0Ah` for input
- **Array Structure**: 352 bytes allocated (16 contacts × 22 bytes each)
- **Sorting Algorithm**: Implements selection sort for ordering contacts by name

## ⚠️ Error Handling

The system includes comprehensive error handling for:
- ❌ Invalid menu choices
- ❌ Full contact list (16/16)
- ❌ Empty contact list
- ❌ Contact not found during search/delete/modify
- ❌ Invalid phone number format (non-digit characters or incorrect length)

## 📖 Usage Example

1.  Run the program in emu8086.
2.  Use the numeric menu to select an operation.
3.  For **adding**: Enter a name (max 10 chars) and a 10-digit phone number.
4.  For **searching/modifying/deleting**: Enter the exact name of the contact.
5.  The system provides feedback and prompts for any invalid inputs.

## 📝 Learning Objectives

This practical work demonstrates proficiency in:
- Low-level memory management
- DOS interrupt programming (`int 21h`)
- String manipulation in assembly
- Algorithm implementation (sorting, searching)
- User interface design for console applications
- Structured programming using macros and procedures
