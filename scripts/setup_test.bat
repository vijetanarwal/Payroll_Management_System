@echo off
REM Payroll Management System - Setup Verification Script for Windows
REM Author: Payroll System Team
REM Date: 2025

setlocal enabledelayedexpansion

echo ==================================================
echo   Payroll Management System - Setup Test        
echo ==================================================
echo.

set TOTAL_TESTS=0
set PASSED_TESTS=0
set FAILED_TESTS=0

REM Test Suite 1: GnuCOBOL Installation
echo Test Suite 1: GnuCOBOL Installation
echo -----------------------------------
set /a TOTAL_TESTS+=1
where cobc >nul 2>&1
if %errorLevel% equ 0 (
    echo [PASS] GnuCOBOL compiler ^(cobc^) is installed
    set /a PASSED_TESTS+=1
    for /f "tokens=*" %%v in ('cobc --version 2^>^&1 ^| findstr /r "^"') do (
        echo [INFO] Version: %%v
        goto :version_done
    )
    :version_done
) else (
    echo [FAIL] GnuCOBOL compiler ^(cobc^) is installed
    set /a FAILED_TESTS+=1
)
echo.

REM Test Suite 2: Required Files
echo Test Suite 2: Required Files
echo ----------------------------
set /a TOTAL_TESTS+=1
if exist "payroll.cbl" (
    echo [PASS] payroll.cbl source file exists
    set /a PASSED_TESTS+=1
) else (
    echo [FAIL] payroll.cbl source file exists
    set /a FAILED_TESTS+=1
)

set /a TOTAL_TESTS+=1
if exist "employee.dat" (
    echo [PASS] employee.dat data file exists
    set /a PASSED_TESTS+=1
) else (
    echo [FAIL] employee.dat data file exists
    set /a FAILED_TESTS+=1
)

set /a TOTAL_TESTS+=1
if exist "payroll.dat" (
    echo [PASS] payroll.dat data file exists
    set /a PASSED_TESTS+=1
) else (
    echo [FAIL] payroll.dat data file exists
    set /a FAILED_TESTS+=1
)

set /a TOTAL_TESTS+=1
if exist "reports.txt" (
    echo [PASS] reports.txt file exists
    set /a PASSED_TESTS+=1
) else (
    echo [FAIL] reports.txt file exists
    set /a FAILED_TESTS+=1
)
echo.

REM Test Suite 3: File Accessibility
echo Test Suite 3: File Accessibility
echo --------------------------------
set /a TOTAL_TESTS+=1
if exist "employee.dat" (
    type employee.dat >nul 2>&1
    if !errorLevel! equ 0 (
        echo [PASS] employee.dat is readable
        set /a PASSED_TESTS+=1
    ) else (
        echo [FAIL] employee.dat is readable
        set /a FAILED_TESTS+=1
    )
) else (
    echo [WARN] employee.dat not found, skipping read test
    set /a FAILED_TESTS+=1
)

set /a TOTAL_TESTS+=1
if exist "payroll.dat" (
    echo test >nul 2>payroll.dat
    if !errorLevel! equ 0 (
        echo [PASS] payroll.dat is writable
        set /a PASSED_TESTS+=1
    ) else (
        echo [FAIL] payroll.dat is writable
        set /a FAILED_TESTS+=1
    )
) else (
    echo [WARN] payroll.dat not found, skipping write test
    set /a FAILED_TESTS+=1
)

set /a TOTAL_TESTS+=1
if exist "reports.txt" (
    echo test >nul 2>reports.txt
    if !errorLevel! equ 0 (
        echo [PASS] reports.txt is writable
        set /a PASSED_TESTS+=1
    ) else (
        echo [FAIL] reports.txt is writable
        set /a FAILED_TESTS+=1
    )
) else (
    echo [WARN] reports.txt not found, skipping write test
    set /a FAILED_TESTS+=1
)
echo.

REM Test Suite 4: Compiled Program
echo Test Suite 4: Compiled Program
echo ------------------------------
set /a TOTAL_TESTS+=1
if exist "payroll.exe" (
    echo [PASS] Compiled executable 'payroll.exe' exists
    set /a PASSED_TESTS+=1
) else (
    if exist "payroll" (
        echo [PASS] Compiled executable 'payroll' exists
        set /a PASSED_TESTS+=1
    ) else (
        echo [FAIL] Compiled executable not found
        set /a FAILED_TESTS+=1
    )
)
echo.

REM Test Suite 5: Data File Validation
echo Test Suite 5: Data File Validation
echo -----------------------------------
if exist "employee.dat" (
    set /a TOTAL_TESTS+=1
    for /f %%a in ('find /c /v "" ^< employee.dat') do set LINE_COUNT=%%a
    if !LINE_COUNT! gtr 0 (
        echo [PASS] employee.dat contains data ^(!LINE_COUNT! lines^)
        set /a PASSED_TESTS+=1
    ) else (
        echo [WARN] employee.dat is empty
        set /a FAILED_TESTS+=1
    )
    
    set /a TOTAL_TESTS+=1
    for /f "delims=" %%l in (employee.dat) do (
        set FIRST_LINE=%%l
        goto :check_length
    )
    :check_length
    set LINE_LEN=0
    set STR=!FIRST_LINE!
    :length_loop
    if defined STR (
        set STR=!STR:~1!
        set /a LINE_LEN+=1
        goto :length_loop
    )
    if !LINE_LEN! geq 80 (
        echo [PASS] employee.dat has proper record format
        set /a PASSED_TESTS+=1
    ) else (
        echo [FAIL] employee.dat format appears incorrect ^(!LINE_LEN! chars, expected 80+^)
        set /a FAILED_TESTS+=1
    )
)
echo.

REM Test Suite 6: Directory Structure
echo Test Suite 6: Directory Structure
echo ----------------------------------
set /a TOTAL_TESTS+=1
if exist "scripts\" (
    echo [PASS] scripts directory exists
    set /a PASSED_TESTS+=1
) else (
    mkdir scripts 2>nul
    if exist "scripts\" (
        echo [PASS] scripts directory created
        set /a PASSED_TESTS+=1
    ) else (
        echo [FAIL] Cannot create scripts directory
        set /a FAILED_TESTS+=1
    )
)

set /a TOTAL_TESTS+=1
echo test >nul 2>test_write.tmp
if %errorLevel% equ 0 (
    echo [PASS] Current directory is writable
    set /a PASSED_TESTS+=1
    del test_write.tmp 2>nul
) else (
    echo [FAIL] Current directory is not writable
    set /a FAILED_TESTS+=1
)
echo.

REM Test Suite 7: COBOL Syntax Check
echo Test Suite 7: COBOL Syntax Check
echo ---------------------------------
where cobc >nul 2>&1
if %errorLevel% equ 0 (
    if exist "payroll.cbl" (
        set /a TOTAL_TESTS+=1
        cobc -fsyntax-only payroll.cbl >nul 2>&1
        if !errorLevel! equ 0 (
            echo [PASS] payroll.cbl syntax is valid
            set /a PASSED_TESTS+=1
        ) else (
            echo [FAIL] payroll.cbl has syntax errors
            set /a FAILED_TESTS+=1
        )
    ) else (
        echo [WARN] Skipping syntax check ^(payroll.cbl not found^)
    )
) else (
    echo [WARN] Skipping syntax check ^(cobc not available^)
)
echo.

REM Test Suite 8: Runtime Quick Test
echo Test Suite 8: Runtime Quick Test
echo ---------------------------------
if exist "payroll.exe" (
    echo [INFO] Attempting quick runtime test...
    set /a TOTAL_TESTS+=1
    REM Try to run program briefly
    echo | payroll.exe >nul 2>&1
    if !errorLevel! leq 1 (
        echo [PASS] Program starts without immediate errors
        set /a PASSED_TESTS+=1
    ) else (
        echo [WARN] Program may have runtime issues
        set /a FAILED_TESTS+=1
    )
) else (
    if exist "payroll" (
        echo [INFO] Attempting quick runtime test...
        set /a TOTAL_TESTS+=1
        echo | payroll >nul 2>&1
        if !errorLevel! leq 1 (
            echo [PASS] Program starts without immediate errors
            set /a PASSED_TESTS+=1
        ) else (
            echo [WARN] Program may have runtime issues
            set /a FAILED_TESTS+=1
        )
    ) else (
        echo [WARN] Skipping runtime test ^(executable not available^)
    )
)
echo.

REM Summary
echo ==================================================
echo               TEST SUMMARY                       
echo ==================================================
echo.
echo Total Tests: %TOTAL_TESTS%
echo Passed: %PASSED_TESTS%
echo Failed: %FAILED_TESTS%
echo.

set /a PASS_PERCENTAGE=(%PASSED_TESTS% * 100) / %TOTAL_TESTS%
echo Success Rate: %PASS_PERCENTAGE%%%
echo.

if %FAILED_TESTS% equ 0 (
    echo [SUCCESS] All tests passed! System is ready to use.
    echo.
    echo To start the Payroll Management System:
    echo   payroll.exe
    echo   or simply: payroll
    echo.
    pause
    exit /b 0
) else (
    echo [ERROR] Some tests failed. Please review the errors above.
    echo.
    echo Common fixes:
    echo   - Run the installation script: scripts\install.bat
    echo   - Check file permissions
    echo   - Recompile: cobc -x -o payroll.exe payroll.cbl
    echo.
    pause
    exit /b 1
)