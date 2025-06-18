# 💼 Payroll Management System (COBOL)

A terminal-based Payroll Management System built using **COBOL**, designed to help organizations manage employee records, calculate salaries, and generate payroll reports efficiently.

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![Status](https://img.shields.io/badge/status-production--ready-brightgreen)
![Platform](https://img.shields.io/badge/platform-Terminal-blue)

---

## 📜 Table of Contents

- [🚀 Features](#-features)
- [📂 Project Structure](#-project-structure)
- [🏗️ System Architecture](#-system-architecture)
- [📊 Salary Calculation](#-salary-calculation)
- [🔧 Requirements](#-requirements)
- [⚙️ Installation & Running](#️-installation--running)
- [💻 Usage](#-usage)
- [📸 Screenshots](#-screenshots)
- [🗂️ Sample Data](#-sample-data)
- [✅ Future Enhancements](#-future-enhancements)
- [🛠️ Troubleshooting](#-troubleshooting)
- [📃 License](#-license)
- [🙋‍♂️ Support](#-support)

---

## 🚀 Features

✅ Add, Edit, Delete, and Search Employee Records  
✅ Calculate Salary with Overtime, Bonus, Deductions, and Tax  
✅ Generate Monthly Reports & Salary Slips  
✅ Secure Admin Login  
✅ Export Data to CSV  
✅ Configurable Tax & Overtime Rates  
✅ Simple CLI-Based Navigation  

---

## 📂 Project Structure

| File / Folder         | Description                               |
|------------------------|-------------------------------------------|
| `main.cbl`             | Main menu and navigation logic            |
| `employee.cbl`         | Employee CRUD operations                  |
| `payroll.cbl`          | Salary calculation logic                  |
| `report.cbl`           | Report generation & CSV export            |
| `auth.cbl`             | Admin authentication module               |
| `employee.dat`         | Flat file storing employee records        |
| `payroll.dat`          | Flat file storing payroll transactions    |
| `reports.txt`          | Output text report                        |
| `employee_export.csv`  | Exported employee data (CSV)              |
| `payroll_export.csv`   | Exported payroll data (CSV)               |

---

## 🏗️ System Architecture

- **Main Menu** – CLI with menu-driven options  
- **Authentication** – Validates admin access  
- **Employee Module** – Add/Edit/Delete/Search  
- **Payroll Module** – Salary computation logic  
- **Reporting Module** – Generates `.txt` and `.csv` reports  
- **File System** – Stores data using flat files  

**Data Flow:**  
`Admin Login → Manage Employees → Process Payroll → Generate Reports → Export Data`

---

## 📊 Salary Calculation


Gross Salary   = Base Salary + (Overtime Hours × Rate) + Bonus  
Tax (10%)      = 0.10 × Gross Salary  
Net Salary     = Gross Salary - Tax - Deductions

**Default Settings:**

* Base Work Month: 22 Days
* Overtime Rate: `$100/hour`
* Tax Rate: `10%`
* Minimum Salary: `$30,000/year`
* Max Overtime: `80 hours/month`

---

## 🔧 Requirements

### Software

* COBOL Compiler (GnuCOBOL recommended)
* Terminal / Command Prompt
* Text Editor (VS Code, Vim, Nano)

### Hardware

* 512MB RAM minimum
* 50MB free disk space

---

## ⚙️ Installation & Running

### 1️⃣ Install GnuCOBOL

**Ubuntu/Debian**

```bash
sudo apt update
sudo apt install gnucobol
```

**macOS**

```bash
brew install gnu-cobol
```

**Windows**
Download GnuCOBOL from [https://gnucobol.sourceforge.io/](https://gnucobol.sourceforge.io/)

---

### 2️⃣ Setup Files

```bash
mkdir payroll-system
cd payroll-system
touch employee.dat payroll.dat reports.txt
```

---

### 3️⃣ Compile the COBOL Source

```bash
cobc -x -o payroll main.cbl employee.cbl payroll.cbl report.cbl auth.cbl
```

### 4️⃣ Run the Program

```bash
./payroll
```

---

## 💻 Usage

### 🛡️ Login

```text
Username: admin  
Password: payroll123
```

### 🧭 Menu Interface

```
+-------------------------------------+
|        PAYROLL MAIN MENU           |
+-------------------------------------+
| 1. Manage Employees                |
| 2. Compute Payroll                 |
| 3. Generate Reports                |
| 4. Exit                            |
+-------------------------------------+
Enter choice:
```

### 🧪 Sample Workflow

**Add Employee**

```
ID: 10001  
Name: John Smith  
Dept: IT  
Base Salary: 75000  
```

**Process Payroll**

```
Days Worked: 22  
Overtime: 8  
Bonus: 1000  
Deductions: 200  
```

**Calculation**

```
Overtime Pay: 8 × $100 = $800  
Gross: $76,800  
Tax: $7,680  
Net Salary: $68,920  
```

---

## 📸 Screenshots

<img width="1121" alt="Screenshot 2025-06-18 at 1 56 12 PM" src="https://github.com/user-attachments/assets/fdc9414c-1a54-47bc-9d1f-7d787bfbd786" />
<img width="1121" alt="Screenshot 2025-06-18 at 1 56 26 PM" src="https://github.com/user-attachments/assets/27676b4e-4148-45fa-93bc-4d1e00fb9418" />


```
Payroll Report - December 2025
ID: 10001 | Name: John Smith  
Gross: $76,800.00 | Net: $68,920.00
```

---

## 🗂️ Sample Data

### `employee.dat`

```text
10001John Smith                  IT                  Software Engineer        75000.00
10002Jane Doe                    HR                  HR Manager               68000.00
```

### `payroll.dat`

```text
10001,22,8,1000,200,7680,76800,68920,12,2025
```

---

## ✅ Future Enhancements

* 🔐 Role-based Authentication
* 📊 Graphical Report Output (CSV/HTML)
* 🌍 Web Interface using COBOL-Web bridge
* 🗃️ VSAM / Indexed File / SQL DB Integration
* 🔁 Backup & Restore functionality
* 🛡️ Better Input Validation & Logging

---

## 🛠️ Troubleshooting

| Issue               | Fix                                                                   |
| ------------------- | --------------------------------------------------------------------- |
| ❌ Compilation Error | Check COBOL syntax and ensure `IDENTIFICATION`, `PROCEDURE` divisions |
| 🗂️ File Error      | Ensure `.dat` files exist and have correct permissions                |
| 🧮 Wrong Output     | Validate inputs and verify salary formula                             |

---

## 📃 License

This project is licensed under the **MIT License** © 2025 **JAGADISH SUNIL PEDNEKAR**.

---

## 🙋‍♂️ Support

If you like this project, give it a ⭐ at [Payroll\_Management\_System](https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System).
For issues or suggestions, open an [Issue](https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System/issues).

---

📅 **Last Updated:** June 2025
📦 **Version:** 1.0
🚀 **Status:** Production Ready



