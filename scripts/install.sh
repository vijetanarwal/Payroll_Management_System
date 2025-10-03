#!/bin/bash

# Payroll Management System - Installation Script for Linux/macOS
# Author: Payroll System Team
# Date: 2025

set -e

echo "=================================================="
echo "  Payroll Management System - Installation      "
echo "=================================================="
echo ""

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored messages
print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_info() {
    echo -e "$1"
}

# Check if running on macOS or Linux
OS_TYPE=$(uname -s)
print_info "Detected OS: $OS_TYPE"
echo ""

# Step 1: Check for GnuCOBOL installation
print_info "Step 1: Checking for GnuCOBOL installation..."
if command -v cobc &> /dev/null; then
    COBOL_VERSION=$(cobc --version | head -n 1)
    print_success "GnuCOBOL is already installed: $COBOL_VERSION"
else
    print_warning "GnuCOBOL not found. Attempting to install..."
    
    if [ "$OS_TYPE" = "Darwin" ]; then
        # macOS installation
        if command -v brew &> /dev/null; then
            print_info "Installing GnuCOBOL via Homebrew..."
            brew install gnu-cobol
            print_success "GnuCOBOL installed successfully"
        else
            print_error "Homebrew not found. Please install Homebrew first:"
            print_info "Visit: https://brew.sh"
            exit 1
        fi
    elif [ "$OS_TYPE" = "Linux" ]; then
        # Linux installation
        if command -v apt-get &> /dev/null; then
            print_info "Installing GnuCOBOL via apt..."
            sudo apt-get update
            sudo apt-get install -y gnucobol
            print_success "GnuCOBOL installed successfully"
        elif command -v yum &> /dev/null; then
            print_info "Installing GnuCOBOL via yum..."
            sudo yum install -y gnucobol
            print_success "GnuCOBOL installed successfully"
        else
            print_error "Package manager not found. Please install GnuCOBOL manually."
            print_info "Visit: https://gnucobol.sourceforge.io/"
            exit 1
        fi
    else
        print_error "Unsupported operating system: $OS_TYPE"
        exit 1
    fi
fi
echo ""

# Step 2: Create necessary directories
print_info "Step 2: Creating project directories..."
mkdir -p scripts
mkdir -p data
print_success "Directories created"
echo ""

# Step 3: Create/verify data files
print_info "Step 3: Setting up data files..."

# Create employee.dat if it doesn't exist or is empty
if [ ! -f employee.dat ] || [ ! -s employee.dat ]; then
    print_info "Creating employee.dat with sample data..."
    cat > employee.dat << 'EOF'
10001John Smith                  IT Department        Software Engineer        75000.00
10002Jane Doe                    HR Department        HR Manager               68000.00
10003Bob Johnson                 Finance Department   Financial Analyst        62000.00
10004Alice Brown                 IT Department        Senior Developer         85000.00
10005Charlie Wilson              Marketing Department Marketing Manager        71000.00
EOF
    print_success "employee.dat created with sample data"
else
    print_success "employee.dat already exists"
fi

# Create payroll.dat if it doesn't exist
if [ ! -f payroll.dat ]; then
    print_info "Creating empty payroll.dat..."
    touch payroll.dat
    print_success "payroll.dat created"
else
    print_success "payroll.dat already exists"
fi

# Create reports.txt if it doesn't exist
if [ ! -f reports.txt ]; then
    print_info "Creating empty reports.txt..."
    touch reports.txt
    print_success "reports.txt created"
else
    print_success "reports.txt already exists"
fi

echo ""

# Step 4: Set file permissions
print_info "Step 4: Setting file permissions..."
chmod 644 employee.dat payroll.dat reports.txt 2>/dev/null || true
chmod 755 payroll.cbl 2>/dev/null || true
print_success "Permissions set"
echo ""

# Step 5: Compile the COBOL program
print_info "Step 5: Compiling COBOL program..."
if [ -f payroll.cbl ]; then
    if cobc -x -o payroll payroll.cbl 2>/dev/null; then
        print_success "Program compiled successfully"
        chmod +x payroll
    else
        print_error "Compilation failed. Please check payroll.cbl for errors."
        print_info "You can compile manually with: cobc -x -o payroll payroll.cbl"
        exit 1
    fi
else
    print_warning "payroll.cbl not found. Skipping compilation."
    print_info "Please ensure payroll.cbl is in the current directory."
fi
echo ""

# Step 6: Verify installation
print_info "Step 6: Verifying installation..."
VERIFICATION_PASSED=true

# Check for compiled executable
if [ -f ./payroll ]; then
    print_success "Executable 'payroll' found"
else
    print_error "Executable 'payroll' not found"
    VERIFICATION_PASSED=false
fi

# Check for data files
for file in employee.dat payroll.dat reports.txt; do
    if [ -f "$file" ]; then
        print_success "File '$file' exists"
    else
        print_error "File '$file' not found"
        VERIFICATION_PASSED=false
    fi
done

echo ""

# Final status
if [ "$VERIFICATION_PASSED" = true ]; then
    print_success "Installation completed successfully!"
    echo ""
    print_info "=================================================="
    print_info "         INSTALLATION COMPLETE                   "
    print_info "=================================================="
    echo ""
    print_info "To run the Payroll Management System:"
    print_info "  ./payroll"
    echo ""
    print_info "Default login credentials:"
    print_info "  Username: admin"
    print_info "  Password: payroll123"
    echo ""
    print_info "For help, see README.md or visit:"
    print_info "  https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System"
    echo ""
else
    print_error "Installation completed with errors."
    print_info "Please check the error messages above and fix any issues."
    exit 1
fi