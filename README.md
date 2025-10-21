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

```text
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

```text
ID: 10001  
Name: John Smith  
Dept: IT  
Base Salary: 75000  
```

**Process Payroll**

```text
Days Worked: 22  
Overtime: 8  
Bonus: 1000  
Deductions: 200  
```

**Calculation**

```text
Overtime Pay: 8 × $100 = $800  
Gross: $76,800  
Tax: $7,680  
Net Salary: $68,920  
```

---

## 📸 Screenshots

<img width="1121" alt="Screenshot 2025-06-18 at 1 56 12 PM" src="https://github.com/user-attachments/assets/fdc9414c-1a54-47bc-9d1f-7d787bfbd786" />
<img width="1121" alt="Screenshot 2025-06-18 at 1 56 26 PM" src="https://github.com/user-attachments/assets/27676b4e-4148-45fa-93bc-4d1e00fb9418" />

```text
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

## ⚙️ Quick Start Installation

### 🚀 Automated Installation (Recommended)

#### Linux/macOS
```bash
# Clone the repository
git clone https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System.git
cd Payroll_Management_System

# Make install script executable
chmod +x scripts/install.sh

# Run installation
./scripts/install.sh
```

#### Windows
```cmd
REM Clone the repository
git clone https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System.git
cd Payroll_Management_System

REM Run installation
scripts\install.bat
```

### 📋 What the Installation Script Does

The automated installation script will:
- ✅ Check for GnuCOBOL installation (and attempt to install if missing)
- ✅ Create necessary directories (`scripts/`, `data/`)
- ✅ Set up data files with sample employee data
- ✅ Compile the COBOL program
- ✅ Set proper file permissions
- ✅ Verify the installation

### 🧪 Verify Your Installation

After running the installation script, verify everything is working:

```bash
# Linux/macOS
./scripts/setup_test.sh

# Windows
scripts\setup_test.bat
```

This will run comprehensive tests to ensure:
- GnuCOBOL is properly installed
- All required files exist
- File permissions are correct
- The program compiles without errors
- Data files have proper format

---

## ⚙️ Manual Installation

If you prefer to install manually or the automated script doesn't work:

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
- Download from [https://gnucobol.sourceforge.io/](https://gnucobol.sourceforge.io/)
- Or use Windows Package Manager:
  ```cmd
  winget install GnuCOBOL
  ```
- Add GnuCOBOL to your system PATH

**Fedora/RHEL**
```bash
sudo yum install gnucobol
```

### 2️⃣ Setup Files

```bash
# Create necessary directories
mkdir -p scripts data

# Create data files
touch employee.dat payroll.dat reports.txt

# Add sample employee data to employee.dat
cat > employee.dat << 'EOF'
10001John Smith                  IT Department        Software Engineer        75000.00
10002Jane Doe                    HR Department        HR Manager               68000.00
10003Bob Johnson                 Finance Department   Financial Analyst        62000.00
10004Alice Brown                 IT Department        Senior Developer         85000.00
10005Charlie Wilson              Marketing Department Marketing Manager        71000.00
EOF
```

### 3️⃣ Compile the COBOL Source

```bash
cobc -x -o payroll payroll.cbl
```

For debugging mode:
```bash
cobc -x -g -o payroll payroll.cbl
```

To check syntax only:
```bash
cobc -fsyntax-only payroll.cbl
```

### 4️⃣ Run the Program

**Linux/macOS:**
```bash
./payroll
```

**Windows:**
```cmd
payroll.exe
```

---

## 🛠️ Comprehensive Troubleshooting

### Installation Issues

#### ❌ "cobc: command not found" or "GnuCOBOL not found"

**Problem:** GnuCOBOL compiler is not installed or not in PATH.

**Solutions:**
1. **Install GnuCOBOL:**
   - Linux: `sudo apt install gnucobol`
   - macOS: `brew install gnu-cobol`
   - Windows: Download from [gnucobol.sourceforge.io](https://gnucobol.sourceforge.io/)

2. **Check PATH (Windows):**
   ```cmd
   echo %PATH%
   ```
   Add GnuCOBOL installation directory to PATH if missing.

3. **Verify installation:**
   ```bash
   cobc --version
   ```

#### ❌ "Permission denied" when running install.sh

**Problem:** Script doesn't have execute permissions.

**Solution:**
```bash
chmod +x scripts/install.sh
./scripts/install.sh
```

#### ❌ "Homebrew not found" on macOS

**Problem:** Homebrew package manager is not installed.

**Solution:**
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

---

### Compilation Errors

#### ❌ Compilation fails with syntax errors

**Problem:** COBOL source file has errors or incompatible syntax.

**Solutions:**
1. **Check syntax only:**
   ```bash
   cobc -fsyntax-only payroll.cbl
   ```

2. **Common issues:**
   - Missing periods (`.`) at end of statements
   - Incorrect column alignment (COBOL is column-sensitive)
   - Reserved word conflicts

3. **Verify file encoding:** Should be UTF-8 or ASCII
   ```bash
   file payroll.cbl
   ```

#### ❌ "error: EMPLOYEE-FILE: File name too long"

**Problem:** File path exceeds system limits.

**Solution:** Move project to a directory with a shorter path.

#### ❌ Linker errors during compilation

**Problem:** Missing system libraries.

**Solutions:**
- **Linux:** Install development tools
  ```bash
  sudo apt install build-essential
  ```
- **macOS:** Install Xcode Command Line Tools
  ```bash
  xcode-select --install
  ```

---

### Runtime Errors

#### ❌ "File not found: employee.dat"

**Problem:** Data file doesn't exist or program can't access it.

**Solutions:**
1. **Create the file:**
   ```bash
   touch employee.dat
   ```

2. **Check current directory:**
   ```bash
   ls -la *.dat
   ```

3. **Run from correct directory:** Ensure you're in the project root
   ```bash
   pwd
   ```

#### ❌ "File status 35" or I/O errors

**Problem:** File permissions or corruption issues.

**Solutions:**
1. **Check permissions:**
   ```bash
   ls -l *.dat
   chmod 644 employee.dat payroll.dat
   ```

2. **Verify file format:** Ensure no binary characters in text files
   ```bash
   file employee.dat
   cat -v employee.dat
   ```

3. **Recreate data files if corrupted:**
   ```bash
   mv employee.dat employee.dat.backup
   ./scripts/install.sh
   ```

#### ❌ "Login Failed" even with correct credentials

**Problem:** File format or encoding issues in authentication.

**Solutions:**
1. **Default credentials:**
   - Username: `admin`
   - Password: `payroll123`

2. **Check for extra spaces:** Ensure no trailing spaces when typing

3. **Recompile the program:**
   ```bash
   cobc -x -o payroll payroll.cbl
   ```

#### ❌ "Invalid numeric data" or calculation errors

**Problem:** Data file format doesn't match COBOL picture clauses.

**Solutions:**
1. **Check data alignment:** Each field must be in correct position
2. **Verify numeric fields:** Should contain only digits and decimal point
3. **Use sample data template:**
   ```
   10001John Smith                  IT Department        Software Engineer        75000.00
   ^    ^                           ^                    ^                        ^
   ID   Name (30 chars)             Dept (20 chars)      Position (25 chars)      Salary
   ```

---

### Platform-Specific Issues

#### 🐧 Linux Issues

**Problem:** "error while loading shared libraries: libcob.so.4"

**Solution:**
```bash
sudo ldconfig
# Or set LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

#### 🍎 macOS Issues

**Problem:** "dyld: Library not loaded"

**Solution:**
```bash
# Reinstall GnuCOBOL
brew reinstall gnu-cobol

# Or set DYLD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH
```

#### 🪟 Windows Issues

**Problem:** "The code execution cannot proceed because libcob-4.dll was not found"

**Solutions:**
1. Reinstall GnuCOBOL
2. Add GnuCOBOL bin directory to PATH
3. Copy required DLLs to program directory

**Problem:** Line endings causing issues

**Solution:** Convert line endings to CRLF
```cmd
dos2unix -k payroll.cbl
```

---

### Performance Issues

#### ❌ Slow file operations

**Problem:** Large data files or disk I/O bottleneck.

**Solutions:**
1. **Check file size:**
   ```bash
   du -h *.dat
   ```

2. **Archive old payroll records:**
   ```bash
   mv payroll.dat payroll_2024.dat
   touch payroll.dat
   ```

3. **Use indexed files** (advanced): Modify file organization in COBOL

#### ❌ Program hangs or freezes

**Problem:** Infinite loop or waiting for input.

**Solutions:**
1. **Force quit:**
   - Linux/macOS: `Ctrl+C`
   - Windows: `Ctrl+Break`

2. **Check for input prompts:** Ensure all ACCEPT statements work correctly

3. **Add timeout to testing:**
   ```bash
   timeout 10s ./payroll
   ```

---

### Data Issues

#### ❌ Reports show incorrect calculations

**Problem:** Tax rate or formula issues.

**Solutions:**
1. **Verify constants:**
   - Default tax rate: 10% (0.10)
   - Overtime rate: $100/hour
   - Working days: 22 days/month

2. **Recalculate manually:**
   ```
   Gross = Base Salary + (OT Hours × $100) + Bonus
   Tax = Gross × 0.10
   Net = Gross - Tax - Deductions
   ```

3. **Check data types:** Ensure numeric fields don't have text

#### ❌ Employee data appears corrupted

**Problem:** Records show garbled or incorrect data.

**Solutions:**
1. **Backup current data:**
   ```bash
   cp employee.dat employee.dat.backup
   ```

2. **Check record format:** Each record must be exactly as defined
   - Employee ID: 5 digits
   - Name: 30 characters (padded with spaces)
   - Department: 20 characters
   - Position: 25 characters
   - Salary: 9 digits with 2 decimals

3. **Manually fix or recreate:** Edit with care to maintain alignment

---

### Getting Help

If you're still experiencing issues:

1. **Check existing issues:** [GitHub Issues](https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System/issues)

2. **Create a new issue** with:
   - Operating system and version
   - GnuCOBOL version (`cobc --version`)
   - Complete error message
   - Steps to reproduce
   - What you've already tried

3. **Contact maintainer:** jsphere16@gmail.com

4. **Community resources:**
   - [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/doc/gnucobol.html)
   - [COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)

---

## 🔧 Advanced Configuration

### Custom Installation Path

If you want to install to a custom directory:

```bash
# Create custom directory
mkdir -p /opt/payroll-system
cd /opt/payroll-system

# Clone and install
git clone https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System.git .
./scripts/install.sh
```

### Environment Variables

You can set these environment variables for customization:

```bash
# Linux/macOS
export PAYROLL_DATA_DIR="/path/to/data"
export PAYROLL_REPORTS_DIR="/path/to/reports"

# Windows
set PAYROLL_DATA_DIR=C:\path\to\data
set PAYROLL_REPORTS_DIR=C:\path\to\reports
```

### Multiple Installations

To run multiple instances:

```bash
# Instance 1
mkdir payroll-dept1
cd payroll-dept1
git clone https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System.git .
./scripts/install.sh

# Instance 2
mkdir payroll-dept2
cd payroll-dept2
git clone https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System.git .
./scripts/install.sh
```

---

## 📊 System Requirements

### Minimum Requirements
- **CPU:** Any x86/x64 processor (1 GHz+)
- **RAM:** 512 MB
- **Disk:** 50 MB free space
- **OS:** Linux, macOS 10.12+, Windows 7+

### Recommended Requirements
- **CPU:** Dual-core processor (2 GHz+)
- **RAM:** 2 GB
- **Disk:** 200 MB free space (for data growth)
- **OS:** Linux (Ubuntu 20.04+), macOS 11+, Windows 10+

### Tested Platforms
- ✅ Ubuntu 20.04, 22.04, 24.04
- ✅ Debian 11, 12
- ✅ macOS Monterey, Ventura, Sonoma
- ✅ Windows 10, 11
- ✅ Fedora 38+
- ✅ RHEL 8, 9

---

## 🔄 Updating the System

### Pull Latest Changes

```bash
# Save your data files
cp employee.dat employee.dat.backup
cp payroll.dat payroll.dat.backup

# Pull updates
git pull origin main

# Recompile
cobc -x -o payroll payroll.cbl

# Restore data if needed
cp employee.dat.backup employee.dat
cp payroll.dat.backup payroll.dat
```

### Migration Between Versions

If data format changes between versions:

1. Export current data to CSV (if feature available)
2. Update to new version
3. Import data back or manually convert

---

## 🧹 Uninstallation

### Complete Removal

**Linux/macOS:**
```bash
# Remove executable
rm -f payroll

# Remove data files (WARNING: This deletes all data!)
rm -f employee.dat payroll.dat reports.txt

# Remove entire directory
cd ..
rm -rf Payroll_Management_System
```

**Windows:**
```cmd
REM Remove executable
del payroll.exe

REM Remove data files
del employee.dat payroll.dat reports.txt

REM Remove directory
cd ..
rmdir /s /q Payroll_Management_System
```

### Keep Data, Remove Program

```bash
# Backup data
mkdir payroll-backup
cp *.dat reports.txt payroll-backup/

# Remove program
rm payroll
rm payroll.cbl enhanced_modules.cbl
```

---

## 📝 Quick Reference Commands

### Compilation
```bash
# Standard compilation
cobc -x -o payroll payroll.cbl

# Debug mode
cobc -x -g -o payroll payroll.cbl

# Syntax check only
cobc -fsyntax-only payroll.cbl

# Verbose output
cobc -x -v -o payroll payroll.cbl
```

### File Management
```bash
# View employee data
cat employee.dat

# Count employees
wc -l employee.dat

# Search for employee
grep "10001" employee.dat

# Backup all data
tar -czf payroll-backup-$(date +%Y%m%d).tar.gz *.dat reports.txt
```

### Testing
```bash
# Run verification
./scripts/setup_test.sh

# Check file integrity
file *.dat

# Validate permissions
ls -la *.dat
```

---

## 🎯 Best Practices

### Data Management
- **Regular backups:** Backup data files weekly
- **Archive old records:** Move old payroll.dat entries to archive files
- **Test on sample data:** Use test data before processing real payroll
- **Validate inputs:** Always verify data before saving

### Security
- **Change default password:** Modify WS-VALID-PASS in payroll.cbl
- **Restrict file permissions:** `chmod 600` for sensitive data
- **Use separate user accounts:** Don't run as root/administrator
- **Audit trail:** Keep logs of who accesses the system

### Performance
- **Keep data files reasonable size:** Archive after 1000+ records
- **Index frequently searched fields:** Consider indexed file organization
- **Regular maintenance:** Clean up temporary files
- **Monitor disk space:** Ensure sufficient space for reports

---

## 📚 Additional Resources

### Documentation
- [CONTRIBUTING.md](CONTRIBUTING.md) - How to contribute
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) - Community guidelines
- [LICENSE](LICENSE) - MIT License terms

### External Resources
- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- [COBOL Standards](https://www.iso.org/standard/74527.html)
- [COBOL Tutorial](https://www.tutorialspoint.com/cobol/index.htm)

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



