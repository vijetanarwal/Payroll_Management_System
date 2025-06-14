IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-MANAGEMENT-SYSTEM.
       AUTHOR. PAYROLL-SYSTEM-DEVELOPER.
       DATE-WRITTEN. 2025.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employee.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-FILE ASSIGN TO "payroll.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "reports.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID               PIC 9(5).
           05 EMP-NAME             PIC X(30).
           05 EMP-DEPARTMENT       PIC X(20).
           05 EMP-POSITION         PIC X(25).
           05 EMP-BASE-SALARY      PIC 9(7)V99.
       
       FD PAYROLL-FILE.
       01 PAYROLL-RECORD.
           05 PAY-EMP-ID           PIC 9(5).
           05 PAY-DAYS-WORKED      PIC 9(2).
           05 PAY-OVERTIME-HOURS   PIC 9(3)V99.
           05 PAY-BONUS            PIC 9(6)V99.
           05 PAY-DEDUCTIONS       PIC 9(6)V99.
           05 PAY-TAX              PIC 9(6)V99.
           05 PAY-GROSS-SALARY     PIC 9(7)V99.
           05 PAY-NET-SALARY       PIC 9(7)V99.
           05 PAY-MONTH            PIC 9(2).
           05 PAY-YEAR             PIC 9(4).
       
       FD REPORT-FILE.
       01 REPORT-RECORD            PIC X(132).
       
       WORKING-STORAGE SECTION.
       01 WS-MENU-CHOICE           PIC 9.
       01 WS-CONTINUE              PIC X VALUE 'Y'.
       01 WS-EOF                   PIC X VALUE 'N'.
       01 WS-FOUND                 PIC X VALUE 'N'.
       01 WS-SEARCH-ID             PIC 9(5).
       01 WS-TEMP-RECORD.
           05 WS-TEMP-ID           PIC 9(5).
           05 WS-TEMP-NAME         PIC X(30).
           05 WS-TEMP-DEPT         PIC X(20).
           05 WS-TEMP-POS          PIC X(25).
           05 WS-TEMP-SALARY       PIC 9(7)V99.
       
       01 WS-PAYROLL-CALC.
           05 WS-CALC-OVERTIME-PAY PIC 9(6)V99.
           05 WS-CALC-GROSS        PIC 9(7)V99.
           05 WS-CALC-TAX          PIC 9(6)V99.
           05 WS-CALC-NET          PIC 9(7)V99.
           05 WS-OVERTIME-RATE     PIC 9(3)V99 VALUE 100.00.
           05 WS-TAX-RATE          PIC V999 VALUE 0.10.
       
       01 WS-PAYROLL-INPUT.
           05 WS-INPUT-ID          PIC 9(5).
           05 WS-INPUT-DAYS        PIC 9(2).
           05 WS-INPUT-OT-HOURS    PIC 9(3)V99.
           05 WS-INPUT-BONUS       PIC 9(6)V99.
           05 WS-INPUT-DEDUCTIONS  PIC 9(6)V99.
           05 WS-INPUT-MONTH       PIC 9(2).
           05 WS-INPUT-YEAR        PIC 9(4).
       
       01 WS-REPORT-COUNTERS.
           05 WS-TOTAL-EMPLOYEES   PIC 9(4) VALUE 0.
           05 WS-TOTAL-GROSS       PIC 9(9)V99 VALUE 0.
           05 WS-TOTAL-NET         PIC 9(9)V99 VALUE 0.
       
       01 WS-AUTH-DATA.
           05 WS-USERNAME          PIC X(10).
           05 WS-PASSWORD          PIC X(10).
           05 WS-VALID-USER        PIC X(10) VALUE "admin".
           05 WS-VALID-PASS        PIC X(10) VALUE "payroll123".
           05 WS-AUTH-STATUS       PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM DISPLAY-HEADER
           PERFORM AUTHENTICATE-USER
           IF WS-AUTH-STATUS = 'Y'
               PERFORM MAIN-MENU-LOOP UNTIL WS-CONTINUE = 'N'
           END-IF
           DISPLAY "Thank you for using Payroll Management System!"
           STOP RUN.
       
       DISPLAY-HEADER.
           DISPLAY "=================================================="
           DISPLAY "         PAYROLL MANAGEMENT SYSTEM v1.0          "
           DISPLAY "=================================================="
           DISPLAY " ".
       
       AUTHENTICATE-USER.
           DISPLAY "LOGIN REQUIRED"
           DISPLAY "Enter Username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "Enter Password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           
           IF WS-USERNAME = WS-VALID-USER AND 
              WS-PASSWORD = WS-VALID-PASS
               MOVE 'Y' TO WS-AUTH-STATUS
               DISPLAY "Login Successful!"
               DISPLAY " "
           ELSE
               DISPLAY "Invalid Credentials. Access Denied."
               MOVE 'N' TO WS-AUTH-STATUS
           END-IF.
       
       MAIN-MENU-LOOP.
           PERFORM DISPLAY-MAIN-MENU
           PERFORM GET-MENU-CHOICE
           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   PERFORM EMPLOYEE-MANAGEMENT
               WHEN 2
                   PERFORM PAYROLL-PROCESSING
               WHEN 3
                   PERFORM REPORTS-MODULE
               WHEN 4
                   MOVE 'N' TO WS-CONTINUE
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.
       
       DISPLAY-MAIN-MENU.
           DISPLAY " "
           DISPLAY "=================== MAIN MENU ==================="
           DISPLAY "1. Employee Management"
           DISPLAY "2. Payroll Processing"
           DISPLAY "3. Reports and Queries"
           DISPLAY "4. Exit System"
           DISPLAY "=================================================="
           DISPLAY " ".
       
       GET-MENU-CHOICE.
           DISPLAY "Enter your choice (1-4): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE.
       
       EMPLOYEE-MANAGEMENT.
           DISPLAY " "
           DISPLAY "========== EMPLOYEE MANAGEMENT MODULE ==========="
           DISPLAY "1. Add New Employee"
           DISPLAY "2. Search Employee"
           DISPLAY "3. Update Employee"
           DISPLAY "4. Delete Employee"
           DISPLAY "5. List All Employees"
           DISPLAY "6. Return to Main Menu"
           DISPLAY "================================================="
           DISPLAY "Enter choice (1-6): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   PERFORM ADD-EMPLOYEE
               WHEN 2
                   PERFORM SEARCH-EMPLOYEE
               WHEN 3
                   PERFORM UPDATE-EMPLOYEE
               WHEN 4
                   PERFORM DELETE-EMPLOYEE
               WHEN 5
                   PERFORM LIST-ALL-EMPLOYEES
               WHEN 6
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Invalid choice."
           END-EVALUATE.
       
       ADD-EMPLOYEE.
           DISPLAY " "
           DISPLAY "========== ADD NEW EMPLOYEE =========="
           
           DISPLAY "Enter Employee ID (5 digits): " WITH NO ADVANCING
           ACCEPT WS-TEMP-ID
           
           PERFORM CHECK-DUPLICATE-ID
           IF WS-FOUND = 'Y'
               DISPLAY "Employee ID already exists!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Enter Employee Name: " WITH NO ADVANCING
           ACCEPT WS-TEMP-NAME
           
           DISPLAY "Enter Department: " WITH NO ADVANCING
           ACCEPT WS-TEMP-DEPT
           
           DISPLAY "Enter Position: " WITH NO ADVANCING
           ACCEPT WS-TEMP-POS
           
           DISPLAY "Enter Base Salary: " WITH NO ADVANCING
           ACCEPT WS-TEMP-SALARY
           
           OPEN EXTEND EMPLOYEE-FILE
           MOVE WS-TEMP-ID TO EMP-ID
           MOVE WS-TEMP-NAME TO EMP-NAME
           MOVE WS-TEMP-DEPT TO EMP-DEPARTMENT
           MOVE WS-TEMP-POS TO EMP-POSITION
           MOVE WS-TEMP-SALARY TO EMP-BASE-SALARY
           
           WRITE EMPLOYEE-RECORD
           CLOSE EMPLOYEE-FILE
           
           DISPLAY "Employee added successfully!"
           DISPLAY " ".
       
       CHECK-DUPLICATE-ID.
           MOVE 'N' TO WS-FOUND
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-ID = WS-TEMP-ID
                           MOVE 'Y' TO WS-FOUND
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           MOVE 'N' TO WS-EOF.
       
       SEARCH-EMPLOYEE.
           DISPLAY " "
           DISPLAY "========== SEARCH EMPLOYEE =========="
           DISPLAY "Enter Employee ID to search: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           MOVE 'N' TO WS-FOUND
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-ID = WS-SEARCH-ID
                           MOVE 'Y' TO WS-FOUND
                           PERFORM DISPLAY-EMPLOYEE-DETAILS
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           
           IF WS-FOUND = 'N'
               DISPLAY "Employee not found."
           END-IF
           
           CLOSE EMPLOYEE-FILE
           MOVE 'N' TO WS-EOF.
       
       DISPLAY-EMPLOYEE-DETAILS.
           DISPLAY " "
           DISPLAY "========== EMPLOYEE DETAILS =========="
           DISPLAY "ID: " EMP-ID
           DISPLAY "Name: " EMP-NAME
           DISPLAY "Department: " EMP-DEPARTMENT
           DISPLAY "Position: " EMP-POSITION
           DISPLAY "Base Salary: $" EMP-BASE-SALARY
           DISPLAY "======================================"
           DISPLAY " ".
       
       UPDATE-EMPLOYEE.
           DISPLAY " "
           DISPLAY "========== UPDATE EMPLOYEE =========="
           DISPLAY "Enter Employee ID to update: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           DISPLAY "Feature under development."
           DISPLAY "Use Delete and Add for now."
           DISPLAY " ".
       
       DELETE-EMPLOYEE.
           DISPLAY " "
           DISPLAY "========== DELETE EMPLOYEE =========="
           DISPLAY "Enter Employee ID to delete: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           DISPLAY "Feature under development."
           DISPLAY "Manual file editing required."
           DISPLAY " ".
       
       LIST-ALL-EMPLOYEES.
           DISPLAY " "
           DISPLAY "========== ALL EMPLOYEES =========="
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY EMP-ID " | " EMP-NAME " | " 
                               EMP-DEPARTMENT " | $" EMP-BASE-SALARY
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           MOVE 'N' TO WS-EOF
           DISPLAY "==================================="
           DISPLAY " ".
       
       PAYROLL-PROCESSING.
           DISPLAY " "
           DISPLAY "========== PAYROLL PROCESSING =========="
           
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-INPUT-ID
           
           PERFORM CHECK-EMPLOYEE-EXISTS
           IF WS-FOUND = 'N'
               DISPLAY "Employee not found!"
               EXIT PARAGRAPH
           END-IF
           
           DISPLAY "Enter Days Worked (max 31): " WITH NO ADVANCING
           ACCEPT WS-INPUT-DAYS
           
           DISPLAY "Enter Overtime Hours: " WITH NO ADVANCING
           ACCEPT WS-INPUT-OT-HOURS
           
           DISPLAY "Enter Bonus Amount: " WITH NO ADVANCING
           ACCEPT WS-INPUT-BONUS
           
           DISPLAY "Enter Deductions: " WITH NO ADVANCING
           ACCEPT WS-INPUT-DEDUCTIONS
           
           DISPLAY "Enter Month (MM): " WITH NO ADVANCING
           ACCEPT WS-INPUT-MONTH
           
           DISPLAY "Enter Year (YYYY): " WITH NO ADVANCING
           ACCEPT WS-INPUT-YEAR
           
           PERFORM CALCULATE-PAYROLL
           PERFORM SAVE-PAYROLL-RECORD
           
           DISPLAY " "
           DISPLAY "========== PAYROLL CALCULATION =========="
           DISPLAY "Employee ID: " WS-INPUT-ID
           DISPLAY "Gross Salary: $" WS-CALC-GROSS
           DISPLAY "Tax (10%): $" WS-CALC-TAX
           DISPLAY "Net Salary: $" WS-CALC-NET
           DISPLAY "========================================"
           DISPLAY " ".
       
       CHECK-EMPLOYEE-EXISTS.
           MOVE 'N' TO WS-FOUND
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-ID = WS-INPUT-ID
                           MOVE 'Y' TO WS-FOUND
                           MOVE EMP-BASE-SALARY TO WS-TEMP-SALARY
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           MOVE 'N' TO WS-EOF.
       
       CALCULATE-PAYROLL.
           COMPUTE WS-CALC-OVERTIME-PAY = 
               WS-INPUT-OT-HOURS * WS-OVERTIME-RATE
           
           COMPUTE WS-CALC-GROSS = 
               WS-TEMP-SALARY + WS-CALC-OVERTIME-PAY + WS-INPUT-BONUS
           
           COMPUTE WS-CALC-TAX = WS-CALC-GROSS * WS-TAX-RATE
           
           COMPUTE WS-CALC-NET = 
               WS-CALC-GROSS - WS-CALC-TAX - WS-INPUT-DEDUCTIONS.
       
       SAVE-PAYROLL-RECORD.
           OPEN EXTEND PAYROLL-FILE
           MOVE WS-INPUT-ID TO PAY-EMP-ID
           MOVE WS-INPUT-DAYS TO PAY-DAYS-WORKED
           MOVE WS-INPUT-OT-HOURS TO PAY-OVERTIME-HOURS
           MOVE WS-INPUT-BONUS TO PAY-BONUS
           MOVE WS-INPUT-DEDUCTIONS TO PAY-DEDUCTIONS
           MOVE WS-CALC-TAX TO PAY-TAX
           MOVE WS-CALC-GROSS TO PAY-GROSS-SALARY
           MOVE WS-CALC-NET TO PAY-NET-SALARY
           MOVE WS-INPUT-MONTH TO PAY-MONTH
           MOVE WS-INPUT-YEAR TO PAY-YEAR
           
           WRITE PAYROLL-RECORD
           CLOSE PAYROLL-FILE
           
           DISPLAY "Payroll record saved successfully!".
       
       REPORTS-MODULE.
           DISPLAY " "
           DISPLAY "========== REPORTS AND QUERIES =========="
           DISPLAY "1. Monthly Payroll Report"
           DISPLAY "2. Employee Salary Slip"
           DISPLAY "3. Top 5 Highest Paid Employees"
           DISPLAY "4. Department Summary"
           DISPLAY "5. Return to Main Menu"
           DISPLAY "========================================"
           DISPLAY "Enter choice (1-5): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   PERFORM MONTHLY-PAYROLL-REPORT
               WHEN 2
                   PERFORM EMPLOYEE-SALARY-SLIP
               WHEN 3
                   PERFORM TOP-EMPLOYEES-REPORT
               WHEN 4
                   PERFORM DEPARTMENT-SUMMARY
               WHEN 5
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Invalid choice."
           END-EVALUATE.
       
       MONTHLY-PAYROLL-REPORT.
           DISPLAY " "
           DISPLAY "Enter Month (MM): " WITH NO ADVANCING
           ACCEPT WS-INPUT-MONTH
           DISPLAY "Enter Year (YYYY): " WITH NO ADVANCING
           ACCEPT WS-INPUT-YEAR
           
           DISPLAY " "
           DISPLAY "========== MONTHLY PAYROLL REPORT =========="
           DISPLAY "Month/Year: " WS-INPUT-MONTH "/" WS-INPUT-YEAR
           DISPLAY "============================================"
           DISPLAY "EMP-ID | GROSS | TAX | NET | DAYS"
           DISPLAY "-------|-------|-----|-----|-----"
           
           MOVE 0 TO WS-TOTAL-EMPLOYEES
           MOVE 0 TO WS-TOTAL-GROSS
           MOVE 0 TO WS-TOTAL-NET
           
           OPEN INPUT PAYROLL-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ PAYROLL-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF PAY-MONTH = WS-INPUT-MONTH AND
                          PAY-YEAR = WS-INPUT-YEAR
                           DISPLAY PAY-EMP-ID " | $" PAY-GROSS-SALARY 
                                   " | $" PAY-TAX " | $" PAY-NET-SALARY
                                   " | " PAY-DAYS-WORKED
                           ADD 1 TO WS-TOTAL-EMPLOYEES
                           ADD PAY-GROSS-SALARY TO WS-TOTAL-GROSS
                           ADD PAY-NET-SALARY TO WS-TOTAL-NET
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PAYROLL-FILE
           
           DISPLAY "============================================"
           DISPLAY "Total Employees: " WS-TOTAL-EMPLOYEES
           DISPLAY "Total Gross: $" WS-TOTAL-GROSS
           DISPLAY "Total Net: $" WS-TOTAL-NET
           DISPLAY "============================================"
           MOVE 'N' TO WS-EOF
           DISPLAY " ".
       
       EMPLOYEE-SALARY-SLIP.
           DISPLAY " "
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           DISPLAY "Enter Month (MM): " WITH NO ADVANCING
           ACCEPT WS-INPUT-MONTH
           DISPLAY "Enter Year (YYYY): " WITH NO ADVANCING
           ACCEPT WS-INPUT-YEAR
           
           MOVE 'N' TO WS-FOUND
           OPEN INPUT PAYROLL-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ PAYROLL-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF PAY-EMP-ID = WS-SEARCH-ID AND
                          PAY-MONTH = WS-INPUT-MONTH AND
                          PAY-YEAR = WS-INPUT-YEAR
                           MOVE 'Y' TO WS-FOUND
                           PERFORM DISPLAY-SALARY-SLIP
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           
           IF WS-FOUND = 'N'
               DISPLAY "No payroll record found for specified criteria."
           END-IF
           
           CLOSE PAYROLL-FILE
           MOVE 'N' TO WS-EOF.
       
       DISPLAY-SALARY-SLIP.
           DISPLAY " "
           DISPLAY "============ SALARY SLIP ============"
           DISPLAY "Employee ID: " PAY-EMP-ID
           DISPLAY "Month/Year: " PAY-MONTH "/" PAY-YEAR
           DISPLAY "Days Worked: " PAY-DAYS-WORKED
           DISPLAY "Overtime Hours: " PAY-OVERTIME-HOURS
           DISPLAY "------------------------------------"
           DISPLAY "Bonus: $" PAY-BONUS
           DISPLAY "Gross Salary: $" PAY-GROSS-SALARY
           DISPLAY "Tax Deduction: $" PAY-TAX
           DISPLAY "Other Deductions: $" PAY-DEDUCTIONS
           DISPLAY "------------------------------------"
           DISPLAY "NET SALARY: $" PAY-NET-SALARY
           DISPLAY "===================================="
           DISPLAY " ".
       
       TOP-EMPLOYEES-REPORT.
           DISPLAY " "
           DISPLAY "========== TOP 5 HIGHEST PAID =========="
           DISPLAY "Feature under development."
           DISPLAY "Requires sorting algorithm implementation."
           DISPLAY "========================================"
           DISPLAY " ".
       
       DEPARTMENT-SUMMARY.
           DISPLAY " "
           DISPLAY "========== DEPARTMENT SUMMARY =========="
           DISPLAY "Feature under development."
           DISPLAY "Will show department-wise salary totals."
           DISPLAY "======================================="
           DISPLAY " ".
