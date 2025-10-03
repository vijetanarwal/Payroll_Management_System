@echo off
REM Payroll Management System - Installation Script for Windows
REM Author: Payroll System Team
REM Date: 2025

setlocal enabledelayedexpansion

echo ==================================================
echo   Payroll Management System - Installation      
echo ==================================================
echo.

REM Check for administrator privileges
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo [WARNING] This script may require administrator privileges.
    echo           Some features might not work without admin rights.
    echo.
    timeout /t 3 >nul
)

REM Step 1: Check for GnuCOBOL installation
echo Step 1: Checking for GnuCOBOL installation...
where cobc >nul 2>&1
if %errorLevel% equ 0 (
    echo [SUCCESS] GnuCOBOL is already installed
    cobc --version | findstr /r "^"
) else (
    echo [ERROR] GnuCOBOL not found!
    echo.
    echo Please install GnuCOBOL manually:
    echo 1. Download from: https://gnucobol.sourceforge.io/
    echo 2. Or use Windows Package Manager: winget install GnuCOBOL
    echo 3. Add GnuCOBOL to your system PATH
    echo.
    echo After installation, run this script again.
    pause
    exit /b 1
)
echo.

REM Step 2: Create necessary directories
echo Step 2: Creating project directories...
if not exist "scripts" mkdir scripts
if not exist "data" mkdir data
echo [SUCCESS] Directories created
echo.

REM Step 3: Create/verify data files
echo Step 3: Setting up data files...

REM Create employee.dat if it doesn't exist or is empty
if not exist "employee.dat" (
    goto CREATE_EMPLOYEE_FILE
) else (
    for %%A in (employee.dat) do (
        if %%~zA equ 0 goto CREATE_EMPLOYEE_FILE
    )
    echo [SUCCESS] employee.dat already exists
    goto CHECK_PAYROLL_FILE
)

:CREATE_EMPLOYEE_FILE
echo Creating employee.dat with sample data...
(
echo 10001John Smith                  IT Department        Software Engineer        75000.00
echo 10002Jane Doe                    HR Department        HR Manager               68000.00
echo 10003Bob Johnson                 Finance Department   Financial Analyst        62000.00
echo 10004Alice Brown                 IT Department        Senior Developer         85000.00
echo 10005Charlie Wilson              Marketing Department Marketing Manager        71000.00
) > employee.dat
echo [SUCCESS] employee.dat created with sample data

:CHECK_PAYROLL_FILE
REM Create payroll.dat if it doesn't exist
if not exist "payroll.dat" (
    echo Creating empty payroll.dat...
    type nul > payroll.dat
    echo [SUCCESS] payroll.dat created
) else (
    echo [SUCCESS] payroll.dat already exists
)

REM Create reports.txt if it doesn't exist
if not exist "reports.txt" (
    echo Creating empty reports.txt...
    type nul > reports.txt
    echo [SUCCESS] reports.txt created
) else (
    echo [SUCCESS] reports.txt already exists
)

echo.

REM Step 4: Compile the COBOL program
echo Step 4: Compiling COBOL program...
if exist "payroll.cbl" (
    cobc -x -o payroll.exe payroll.cbl 2>nul
    if %errorLevel% equ 0 (
        echo [SUCCESS] Program compiled successfully
    ) else (
        echo [ERROR] Compilation failed. Please check payroll.cbl for errors.
        echo You can compile manually with: cobc -x -o payroll.exe payroll.cbl
        pause
        exit /b 1
    )
) else (
    echo [WARNING] payroll.cbl not found. Skipping compilation.
    echo Please ensure payroll.cbl is in the current directory.
)
echo.

REM Step 5: Verify installation
echo Step 5: Verifying installation...
set VERIFICATION_PASSED=1

REM Check for compiled executable
if exist "payroll.exe" (
    echo [SUCCESS] Executable 'payroll.exe' found
) else (
    echo [ERROR] Executable 'payroll.exe' not found
    set VERIFICATION_PASSED=0
)

REM Check for data files
for %%F in (employee.dat payroll.dat reports.txt) do (
    if exist "%%F" (
        echo [SUCCESS] File '%%F' exists
    ) else (
        echo [ERROR] File '%%F' not found
        set VERIFICATION_PASSED=0
    )
)

echo.

REM Final status
if !VERIFICATION_PASSED! equ 1 (
    echo [SUCCESS] Installation completed successfully!
    echo.
    echo ==================================================
    echo          INSTALLATION COMPLETE                   
    echo ==================================================
    echo.
    echo To run the Payroll Management System:
    echo   payroll.exe
    echo   or simply: payroll
    echo.
    echo Default login credentials:
    echo   Username: admin
    echo   Password: payroll123
    echo.
    echo For help, see README.md or visit:
    echo   https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System
    echo.
) else (
    echo [ERROR] Installation completed with errors.
    echo Please check the error messages above and fix any issues.
    pause
    exit /b 1
)

pause