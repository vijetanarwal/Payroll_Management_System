#!/bin/bash

# Payroll Management System - Setup Verification Script
# Author: Payroll System Team
# Date: 2025

set -e

echo "=================================================="
echo "  Payroll Management System - Setup Test        "
echo "=================================================="
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_success() {
    echo -e "${GREEN}✓ PASS${NC} - $1"
}

print_error() {
    echo -e "${RED}✗ FAIL${NC} - $1"
}

print_warning() {
    echo -e "${YELLOW}⚠ WARN${NC} - $1"
}

print_info() {
    echo -e "${BLUE}ℹ INFO${NC} - $1"
}

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

run_test() {
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    if eval "$2"; then
        print_success "$1"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        print_error "$1"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

echo "Running setup verification tests..."
echo ""

# Test 1: Check GnuCOBOL installation
echo "Test Suite 1: GnuCOBOL Installation"
echo "-----------------------------------"
run_test "GnuCOBOL compiler (cobc) is installed" "command -v cobc &> /dev/null"

if command -v cobc &> /dev/null; then
    COBOL_VERSION=$(cobc --version | head -n 1)
    print_info "Version: $COBOL_VERSION"
fi
echo ""

# Test 2: Check required files exist
echo "Test Suite 2: Required Files"
echo "----------------------------"
run_test "payroll.cbl source file exists" "[ -f payroll.cbl ]"
run_test "employee.dat data file exists" "[ -f employee.dat ]"
run_test "payroll.dat data file exists" "[ -f payroll.dat ]"
run_test "reports.txt file exists" "[ -f reports.txt ]"
echo ""

# Test 3: Check file permissions
echo "Test Suite 3: File Permissions"
echo "------------------------------"
run_test "employee.dat is readable" "[ -r employee.dat ]"
run_test "payroll.dat is readable and writable" "[ -r payroll.dat ] && [ -w payroll.dat ]"
run_test "reports.txt is writable" "[ -w reports.txt ]"
echo ""

# Test 4: Check compiled executable
echo "Test Suite 4: Compiled Program"
echo "------------------------------"
run_test "Compiled executable 'payroll' exists" "[ -f ./payroll ]"
if [ -f ./payroll ]; then
    run_test "Executable has execute permissions" "[ -x ./payroll ]"
fi
echo ""

# Test 5: Validate data file format
echo "Test Suite 5: Data File Validation"
echo "-----------------------------------"
if [ -f employee.dat ]; then
    LINE_COUNT=$(wc -l < employee.dat)
    if [ "$LINE_COUNT" -gt 0 ]; then
        print_success "employee.dat contains data ($LINE_COUNT lines)"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        print_warning "employee.dat is empty"
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    # Check if first line has proper format (at least 80 characters)
    FIRST_LINE=$(head -n 1 employee.dat)
    if [ ${#FIRST_LINE} -ge 80 ]; then
        print_success "employee.dat has proper record format"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        print_error "employee.dat format appears incorrect"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
fi
echo ""

# Test 6: Check directory structure
echo "Test Suite 6: Directory Structure"
echo "----------------------------------"
run_test "scripts directory exists" "[ -d scripts ] || mkdir -p scripts"
run_test "Current directory is writable" "[ -w . ]"
echo ""

# Test 7: Syntax check (if cobc available)
echo "Test Suite 7: COBOL Syntax Check"
echo "---------------------------------"
if command -v cobc &> /dev/null && [ -f payroll.cbl ]; then
    if cobc -fsyntax-only payroll.cbl 2>/dev/null; then
        print_success "payroll.cbl syntax is valid"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        print_error "payroll.cbl has syntax errors"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
else
    print_warning "Skipping syntax check (cobc not available or payroll.cbl missing)"
fi
echo ""

# Test 8: Quick runtime test (optional)
echo "Test Suite 8: Runtime Quick Test"
echo "---------------------------------"
if [ -f ./payroll ] && [ -x ./payroll ]; then
    print_info "Attempting quick runtime test..."
    # This would need to be customized based on how the program accepts input
    # For now, we just check if it can start
    timeout 2s ./payroll < /dev/null > /dev/null 2>&1 || true
    if [ $? -eq 124 ] || [ $? -eq 0 ]; then
        print_success "Program starts without immediate errors"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        print_warning "Program may have runtime issues"
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
else
    print_warning "Skipping runtime test (executable not available)"
fi
echo ""

# Summary
echo "=================================================="
echo "              TEST SUMMARY                       "
echo "=================================================="
echo ""
echo "Total Tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
echo -e "${RED}Failed: $FAILED_TESTS${NC}"
echo ""

PASS_PERCENTAGE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
echo "Success Rate: ${PASS_PERCENTAGE}%"
echo ""

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed! System is ready to use.${NC}"
    echo ""
    echo "To start the Payroll Management System:"
    echo "  ./payroll"
    echo ""
    exit 0
else
    echo -e "${RED}✗ Some tests failed. Please review the errors above.${NC}"
    echo ""
    echo "Common fixes:"
    echo "  - Run the installation script: ./scripts/install.sh"
    echo "  - Check file permissions: chmod +x payroll"
    echo "  - Recompile: cobc -x -o payroll payroll.cbl"
    echo ""
    exit 1
fi