       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENHANCED-MODULES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employee.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "temp_employee.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EMPLOYEE-CSV ASSIGN TO "employee_export.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-FILE ASSIGN TO "payroll.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-CSV ASSIGN TO "payroll_export.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID            PIC 9(5).
           05 EMP-NAME          PIC X(30).
           05 EMP-DEPARTMENT    PIC X(20).
           05 EMP-POSITION      PIC X(20).
           05 EMP-BASE-SALARY   PIC 9(7)V99.

       FD TEMP-FILE.
       01 TEMP-EMPLOYEE-RECORD PIC X(100).

       FD EMPLOYEE-CSV.
       01 CSV-EMP-RECORD       PIC X(100).

       FD PAYROLL-FILE.
       01 PAYROLL-RECORD.
           05 PAY-EMP-ID        PIC 9(5).
           05 PAY-DAYS-WORKED   PIC 99.
           05 PAY-OVERTIME-HOURS PIC 99.
           05 PAY-BONUS         PIC 9(5)V99.
           05 PAY-DEDUCTIONS    PIC 9(5)V99.
           05 PAY-TAX           PIC 9(5)V99.
           05 PAY-GROSS-SALARY  PIC 9(7)V99.
           05 PAY-NET-SALARY    PIC 9(7)V99.
           05 PAY-MONTH         PIC X(10).
           05 PAY-YEAR          PIC 9(4).

       FD PAYROLL-CSV.
       01 CSV-PAYROLL-RECORD   PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-SEARCH-ID         PIC 9(5).
       01 WS-FOUND             PIC X VALUE 'N'.
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-TEMP-NAME         PIC X(30).
       01 WS-TEMP-DEPT         PIC X(20).
       01 WS-TEMP-POS          PIC X(20).
       01 WS-TEMP-SALARY       PIC 9(7)V99.
       01 WS-CONTINUE          PIC X.
       01 WS-MENU-CHOICE       PIC 9.
       01 REPORT-RECORD        PIC X(100).
       01 WS-TOTAL-EMPLOYEES   PIC 9(3) VALUE 0.
       01 WS-TOTAL-GROSS       PIC 9(7)V99 VALUE 0.
       01 WS-CALC-GROSS        PIC 9(7)V99 VALUE 0.
       01 WS-EMP-COUNT         PIC 9(3) VALUE 0.

       01 WS-EMPLOYEE-ARRAY.
           05 WS-EMP-TABLE OCCURS 100 TIMES.
               10 WS-EMP-TAB-ID        PIC 9(5).
               10 WS-EMP-TAB-NAME      PIC X(30).
               10 WS-EMP-TAB-DEPT      PIC X(20).
               10 WS-EMP-TAB-SALARY    PIC 9(7)V99.

       01 WS-I                 PIC 9(3).
       01 WS-J                 PIC 9(3).
       01 WS-TEMP-EMP-RECORD.
           05 WS-TEMP-EMP-ID       PIC 9(5).
           05 WS-TEMP-EMP-NAME     PIC X(30).
           05 WS-TEMP-EMP-DEPT     PIC X(20).
           05 WS-TEMP-EMP-SALARY   PIC 9(7)V99.

       PROCEDURE DIVISION.

       *> ========== INCLUDE YOUR MODULES BELOW ==========
       *> Paste all previously defined modules here from UPDATE-EMPLOYEE-ENHANCED
       *> to EXPORT-PAYROLL-CSV without needing to change logic.

       *> (Copy and paste the rest of your enhanced module procedures here)

       STOP RUN.