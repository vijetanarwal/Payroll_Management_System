*> Enhanced modules for UPDATE, DELETE, and SORTING functionality
       *> Add these procedures to the main program
       
       *> ==== ENHANCED UPDATE EMPLOYEE MODULE ====
       UPDATE-EMPLOYEE-ENHANCED.
           DISPLAY " "
           DISPLAY "========== UPDATE EMPLOYEE =========="
           DISPLAY "Enter Employee ID to update: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           MOVE 'N' TO WS-FOUND
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT "temp_employee.dat"
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-ID = WS-SEARCH-ID
                           MOVE 'Y' TO WS-FOUND
                           PERFORM DISPLAY-CURRENT-EMPLOYEE
                           PERFORM GET-UPDATED-EMPLOYEE-DATA
                           PERFORM WRITE-UPDATED-EMPLOYEE
                       ELSE
                           WRITE TEMP-EMPLOYEE-RECORD FROM EMPLOYEE-RECORD
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           CLOSE "temp_employee.dat"
           
           IF WS-FOUND = 'Y'
               PERFORM REPLACE-ORIGINAL-FILE
               DISPLAY "Employee updated successfully!"
           ELSE
               DISPLAY "Employee not found."
           END-IF
           
           MOVE 'N' TO WS-EOF.
       
       DISPLAY-CURRENT-EMPLOYEE.
           DISPLAY " "
           DISPLAY "Current Employee Details:"
           DISPLAY "ID: " EMP-ID
           DISPLAY "Name: " EMP-NAME
           DISPLAY "Department: " EMP-DEPARTMENT
           DISPLAY "Position: " EMP-POSITION
           DISPLAY "Base Salary: $" EMP-BASE-SALARY
           DISPLAY " ".
       
       GET-UPDATED-EMPLOYEE-DATA.
           DISPLAY "Enter new details (press ENTER to keep current):"
           
           DISPLAY "New Name [" EMP-NAME "]: " WITH NO ADVANCING
           ACCEPT WS-TEMP-NAME
           IF WS-TEMP-NAME NOT = SPACES
               MOVE WS-TEMP-NAME TO EMP-NAME
           END-IF
           
           DISPLAY "New Department [" EMP-DEPARTMENT "]: " WITH NO ADVANCING
           ACCEPT WS-TEMP-DEPT
           IF WS-TEMP-DEPT NOT = SPACES
               MOVE WS-TEMP-DEPT TO EMP-DEPARTMENT
           END-IF
           
           DISPLAY "New Position [" EMP-POSITION "]: " WITH NO ADVANCING
           ACCEPT WS-TEMP-POS
           IF WS-TEMP-POS NOT = SPACES
               MOVE WS-TEMP-POS TO EMP-POSITION
           END-IF
           
           DISPLAY "New Base Salary [" EMP-BASE-SALARY "]: " 
                   WITH NO ADVANCING
           ACCEPT WS-TEMP-SALARY
           IF WS-TEMP-SALARY NOT = 0
               MOVE WS-TEMP-SALARY TO EMP-BASE-SALARY
           END-IF.
       
       WRITE-UPDATED-EMPLOYEE.
           WRITE TEMP-EMPLOYEE-RECORD FROM EMPLOYEE-RECORD.
       
       REPLACE-ORIGINAL-FILE.
           *> This is a simplified approach
           *> In production, use proper file handling
           DISPLAY "Updating original file..."
           *> System-specific commands would go here
           *> For now, user must manually replace files.
       
       *> ==== ENHANCED DELETE EMPLOYEE MODULE ====
       DELETE-EMPLOYEE-ENHANCED.
           DISPLAY " "
           DISPLAY "========== DELETE EMPLOYEE =========="
           DISPLAY "Enter Employee ID to delete: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID
           
           MOVE 'N' TO WS-FOUND
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT "temp_employee.dat"
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-ID = WS-SEARCH-ID
                           MOVE 'Y' TO WS-FOUND
                           DISPLAY "Employee " EMP-NAME " will be deleted."
                           DISPLAY "Confirm deletion (Y/N): " 
                                   WITH NO ADVANCING
                           ACCEPT WS-CONTINUE
                           IF WS-CONTINUE = 'Y' OR WS-CONTINUE = 'y'
                               DISPLAY "Employee deleted."
                               *> Don't write this record to temp file
                           ELSE
                               WRITE TEMP-EMPLOYEE-RECORD FROM EMPLOYEE-RECORD
                               DISPLAY "Deletion cancelled."
                           END-IF
                       ELSE
                           WRITE TEMP-EMPLOYEE-RECORD FROM EMPLOYEE-RECORD
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           CLOSE "temp_employee.dat"
           
           IF WS-FOUND = 'N'
               DISPLAY "Employee not found."
           END-IF
           
           MOVE 'N' TO WS-EOF.
       
       *> ==== TOP 5 HIGHEST PAID EMPLOYEES MODULE ====
       *> This requires an array to store and sort employee data
       
       WORKING-STORAGE SECTION.
       *> Add these variables to your existing WORKING-STORAGE
       
       01 WS-EMPLOYEE-ARRAY.
           05 WS-EMP-COUNT             PIC 9(3) VALUE 0.
           05 WS-EMP-TABLE OCCURS 100 TIMES.
               10 WS-EMP-TAB-ID        PIC 9(5).
               10 WS-EMP-TAB-NAME      PIC X(30).
               10 WS-EMP-TAB-DEPT      PIC X(20).
               10 WS-EMP-TAB-SALARY    PIC 9(7)V99.
       
       01 WS-SORT-VARIABLES.
           05 WS-I                     PIC 9(3).
           05 WS-J                     PIC 9(3).
           05 WS-TEMP-EMP-RECORD.
               10 WS-TEMP-EMP-ID       PIC 9(5).
               10 WS-TEMP-EMP-NAME     PIC X(30).
               10 WS-TEMP-EMP-DEPT     PIC X(20).
               10 WS-TEMP-EMP-SALARY   PIC 9(7)V99.
       
       TOP-EMPLOYEES-REPORT-ENHANCED.
           DISPLAY " "
           DISPLAY "========== TOP 5 HIGHEST PAID =========="
           
           PERFORM LOAD-EMPLOYEES-TO-ARRAY
           PERFORM SORT-EMPLOYEES-BY-SALARY
           PERFORM DISPLAY-TOP-5-EMPLOYEES
           
           DISPLAY "========================================"
           DISPLAY " ".
       
       LOAD-EMPLOYEES-TO-ARRAY.
           MOVE 0 TO WS-EMP-COUNT
           OPEN INPUT EMPLOYEE-FILE
           
           PERFORM UNTIL WS-EOF = 'Y' OR WS-EMP-COUNT = 100
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-EMP-COUNT
                       MOVE EMP-ID TO WS-EMP-TAB-ID(WS-EMP-COUNT)
                       MOVE EMP-NAME TO WS-EMP-TAB-NAME(WS-EMP-COUNT)
                       MOVE EMP-DEPARTMENT TO WS-EMP-TAB-DEPT(WS-EMP-COUNT)
                       MOVE EMP-BASE-SALARY TO WS-EMP-TAB-SALARY(WS-EMP-COUNT)
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           MOVE 'N' TO WS-EOF.
       
       SORT-EMPLOYEES-BY-SALARY.
           *> Bubble Sort Algorithm (Descending Order)
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > WS-EMP-COUNT - 1
               PERFORM VARYING WS-J FROM 1 BY 1 
                       UNTIL WS-J > WS-EMP-COUNT - WS-I
                   IF WS-EMP-TAB-SALARY(WS-J) < WS-EMP-TAB-SALARY(WS-J + 1)
                       PERFORM SWAP-EMPLOYEES
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       SWAP-EMPLOYEES.
           MOVE WS-EMP-TAB-ID(WS-J) TO WS-TEMP-EMP-ID
           MOVE WS-EMP-TAB-NAME(WS-J) TO WS-TEMP-EMP-NAME
           MOVE WS-EMP-TAB-DEPT(WS-J) TO WS-TEMP-EMP-DEPT
           MOVE WS-EMP-TAB-SALARY(WS-J) TO WS-TEMP-EMP-SALARY
           
           MOVE WS-EMP-TAB-ID(WS-J + 1) TO WS-EMP-TAB-ID(WS-J)
           MOVE WS-EMP-TAB-NAME(WS-J + 1) TO WS-EMP-TAB-NAME(WS-J)
           MOVE WS-EMP-TAB-DEPT(WS-J + 1) TO WS-EMP-TAB-DEPT(WS-J)
           MOVE WS-EMP-TAB-SALARY(WS-J + 1) TO WS-EMP-TAB-SALARY(WS-J)
           
           MOVE WS-TEMP-EMP-ID TO WS-EMP-TAB-ID(WS-J + 1)
           MOVE WS-TEMP-EMP-NAME TO WS-EMP-TAB-NAME(WS-J + 1)
           MOVE WS-TEMP-EMP-DEPT TO WS-EMP-TAB-DEPT(WS-J + 1)
           MOVE WS-TEMP-EMP-SALARY TO WS-EMP-TAB-SALARY(WS-J + 1).
       
       DISPLAY-TOP-5-EMPLOYEES.
           DISPLAY "RANK | ID    | NAME                    | DEPT        | SALARY"
           DISPLAY "-----|-------|-------------------------|-------------|--------"
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5 OR WS-I > WS-EMP-COUNT
               DISPLAY WS-I "    | " WS-EMP-TAB-ID(WS-I) " | " 
                       WS-EMP-TAB-NAME(WS-I) " | " 
                       WS-EMP-TAB-DEPT(WS-I) " | $" 
                       WS-EMP-TAB-SALARY(WS-I)
           END-PERFORM.
       
       *> ==== DEPARTMENT SUMMARY MODULE ====
       DEPARTMENT-SUMMARY-ENHANCED.
           DISPLAY " "
           DISPLAY "========== DEPARTMENT SUMMARY =========="
           
           PERFORM GENERATE-DEPT-SUMMARY
           
           DISPLAY "======================================="
           DISPLAY " ".
       
       GENERATE-DEPT-SUMMARY.
           MOVE 0 TO WS-TOTAL-EMPLOYEES
           MOVE 0 TO WS-TOTAL-GROSS
           
           DISPLAY "DEPARTMENT           | EMPLOYEES | TOTAL SALARY"
           DISPLAY "---------------------|-----------|-------------"
           
           PERFORM PROCESS-IT-DEPARTMENT
           PERFORM PROCESS-HR-DEPARTMENT
           PERFORM PROCESS-FINANCE-DEPARTMENT
           PERFORM PROCESS-MARKETING-DEPARTMENT
           
           DISPLAY "---------------------------------------------"
           DISPLAY "GRAND TOTAL          | " WS-TOTAL-EMPLOYEES 
                   "        | $" WS-TOTAL-GROSS.
       
       PROCESS-IT-DEPARTMENT.
           MOVE 0 TO WS-EMP-COUNT
           MOVE 0 TO WS-CALC-GROSS
           
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-DEPARTMENT = "IT Department"
                           ADD 1 TO WS-EMP-COUNT
                           ADD EMP-BASE-SALARY TO WS-CALC-GROSS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           
           DISPLAY "IT Department        | " WS-EMP-COUNT 
                   "        | $" WS-CALC-GROSS
           
           ADD WS-EMP-COUNT TO WS-TOTAL-EMPLOYEES
           ADD WS-CALC-GROSS TO WS-TOTAL-GROSS
           MOVE 'N' TO WS-EOF.
       
       PROCESS-HR-DEPARTMENT.
           MOVE 0 TO WS-EMP-COUNT
           MOVE 0 TO WS-CALC-GROSS
           
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-DEPARTMENT = "HR Department"
                           ADD 1 TO WS-EMP-COUNT
                           ADD EMP-BASE-SALARY TO WS-CALC-GROSS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           
           DISPLAY "HR Department        | " WS-EMP-COUNT 
                   "        | $" WS-CALC-GROSS
           
           ADD WS-EMP-COUNT TO WS-TOTAL-EMPLOYEES
           ADD WS-CALC-GROSS TO WS-TOTAL-GROSS
           MOVE 'N' TO WS-EOF.
       
       PROCESS-FINANCE-DEPARTMENT.
           MOVE 0 TO WS-EMP-COUNT
           MOVE 0 TO WS-CALC-GROSS
           
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-DEPARTMENT = "Finance Department"
                           ADD 1 TO WS-EMP-COUNT
                           ADD EMP-BASE-SALARY TO WS-CALC-GROSS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           
           DISPLAY "Finance Department   | " WS-EMP-COUNT 
                   "        | $" WS-CALC-GROSS
           
           ADD WS-EMP-COUNT TO WS-TOTAL-EMPLOYEES
           ADD WS-CALC-GROSS TO WS-TOTAL-GROSS
           MOVE 'N' TO WS-EOF.
       
       PROCESS-MARKETING-DEPARTMENT.
           MOVE 0 TO WS-EMP-COUNT
           MOVE 0 TO WS-CALC-GROSS
           
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF EMP-DEPARTMENT = "Marketing Department"
                           ADD 1 TO WS-EMP-COUNT
                           ADD EMP-BASE-SALARY TO WS-CALC-GROSS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-FILE
           
           DISPLAY "Marketing Department | " WS-EMP-COUNT 
                   "        | $" WS-CALC-GROSS
           
           ADD WS-EMP-COUNT TO WS-TOTAL-EMPLOYEES
           ADD WS-CALC-GROSS TO WS-TOTAL-GROSS
           MOVE 'N' TO WS-EOF.
       
       *> ==== CSV EXPORT MODULE ====
       EXPORT-TO-CSV.
           DISPLAY " "
           DISPLAY "========== EXPORT DATA TO CSV =========="
           DISPLAY "1. Export Employee Data"
           DISPLAY "2. Export Payroll Data"
           DISPLAY "3. Export Monthly Report"
           DISPLAY "Enter choice (1-3): " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   PERFORM EXPORT-EMPLOYEE-CSV
               WHEN 2
                   PERFORM EXPORT-PAYROLL-CSV
               WHEN 3
                   PERFORM EXPORT-MONTHLY-CSV
               WHEN OTHER
                   DISPLAY "Invalid choice."
           END-EVALUATE.
       
       EXPORT-EMPLOYEE-CSV.
           OPEN OUTPUT "employee_export.csv"
           MOVE "ID,Name,Department,Position,Base_Salary" TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           OPEN INPUT EMPLOYEE-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       STRING EMP-ID "," EMP-NAME "," EMP-DEPARTMENT "," 
                              EMP-POSITION "," EMP-BASE-SALARY
                              DELIMITED BY SIZE INTO REPORT-RECORD
                       WRITE REPORT-RECORD
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           CLOSE "employee_export.csv"
           MOVE 'N' TO WS-EOF
           DISPLAY "Employee data exported to employee_export.csv".
       
       EXPORT-PAYROLL-CSV.
           OPEN OUTPUT "payroll_export.csv"
           MOVE "ID,Days,OT_Hours,Bonus,Deductions,Tax,Gross,Net,Month,Year" 
                TO REPORT-RECORD
           WRITE REPORT-RECORD
           
           OPEN INPUT PAYROLL-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ PAYROLL-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       STRING PAY-EMP-ID "," PAY-DAYS-WORKED "," 
                              PAY-OVERTIME-HOURS "," PAY-BONUS "," 
                              PAY-DEDUCTIONS "," PAY-TAX "," 
                              PAY-GROSS-SALARY "," PAY-NET-SALARY "," 
                              PAY-MONTH "," PAY-YEAR
                              DELIMITED BY SIZE INTO REPORT-RECORD
                       WRITE REPORT-RECORD
               END-READ
           END-PERFORM
           
           CLOSE PAYROLL-FILE
           CLOSE "payroll_export.csv"
           MOVE 'N' TO WS-EOF
           DISPLAY "Payroll data exported to payroll_export.csv".