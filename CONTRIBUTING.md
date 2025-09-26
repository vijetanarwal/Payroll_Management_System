# Contributing to Payroll Management System

Thank you for your interest in contributing to our COBOL-based Payroll Management System! This guide will help you get started with contributing to the project.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Contribution Guidelines](#contribution-guidelines)
- [Coding Standards](#coding-standards)
- [Commit Message Guidelines](#commit-message-guidelines)
- [Pull Request Process](#pull-request-process)
- [Issue Reporting](#issue-reporting)
- [Testing Guidelines](#testing-guidelines)

## Getting Started

### Prerequisites

Before you begin, ensure you have the following installed:
- GnuCOBOL compiler
- Git
- A text editor (VS Code, Vim, Nano, etc.)
- Terminal/Command line access

### Development Setup

1. **Fork the repository**
   ```bash
   # Click the Fork button on GitHub, then clone your fork
   git clone https://github.com/YOUR-USERNAME/Payroll_Management_System.git
   cd Payroll_Management_System
   ```

2. **Set up the upstream remote**
   ```bash
   git remote add upstream https://github.com/JAGADISHSUNILPEDNEKAR/Payroll_Management_System.git
   ```

3. **Install GnuCOBOL** (if not already installed)
   ```bash
   # Ubuntu/Debian
   sudo apt update && sudo apt install gnucobol
   
   # macOS
   brew install gnu-cobol
   ```

4. **Compile and test the system**
   ```bash
   cobc -x -o payroll payroll.cbl
   ./payroll
   ```

## Contribution Guidelines

### Types of Contributions Welcome

- Bug fixes
- Feature enhancements
- Documentation improvements
- Test case additions
- Code refactoring
- Performance optimizations
- UI/UX improvements

### Branching Strategy

- `main` - Production-ready code
- `develop` - Integration branch for features
- `feature/feature-name` - New features
- `bugfix/bug-description` - Bug fixes
- `docs/documentation-update` - Documentation changes

### Workflow

1. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes**
   - Write clean, readable code
   - Follow COBOL coding standards
   - Add comments where necessary
   - Update documentation if needed

3. **Test your changes**
   ```bash
   cobc -x -o payroll payroll.cbl
   ./payroll
   # Test all functionality manually
   ```

4. **Commit your changes**
   ```bash
   git add .
   git commit -m "feat: add employee search by department"
   ```

5. **Push and create PR**
   ```bash
   git push origin feature/your-feature-name
   ```

## Coding Standards

### COBOL Conventions

- Use uppercase for COBOL reserved words
- Use descriptive variable names with hyphens (e.g., `EMPLOYEE-NAME`)
- Align code properly for readability
- Use consistent indentation (4 spaces)
- Add comments for complex logic

### File Organization

- Keep modules separate and focused
- Use consistent naming conventions
- Maintain clean data file structures
- Document file formats in comments

### Example Code Format

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE-MODULE.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-EMPLOYEE-RECORD.
    05 WS-EMP-ID         PIC 9(5).
    05 WS-EMP-NAME       PIC X(30).
    05 WS-EMP-SALARY     PIC 9(7)V99.

PROCEDURE DIVISION.
MAIN-PARAGRAPH.
    DISPLAY "Processing employee data..."
    PERFORM PROCESS-EMPLOYEE-DATA
    STOP RUN.

PROCESS-EMPLOYEE-DATA.
    *> Add your logic here
    CONTINUE.
```

## Commit Message Guidelines

We follow conventional commits format:

```
<type>(<scope>): <description>

[optional body]

[optional footer(s)]
```

### Types

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

### Examples

```
feat(payroll): add overtime calculation validation
fix(employee): resolve duplicate ID validation bug
docs(readme): update installation instructions
refactor(reports): simplify monthly report generation
```

## Pull Request Process

### Before Submitting

- [ ] Code compiles without errors
- [ ] All existing functionality still works
- [ ] New features are properly tested
- [ ] Documentation is updated if needed
- [ ] Commit messages follow guidelines
- [ ] Code follows project standards

### PR Description Template

```markdown
## Description
Brief description of changes made.

## Type of Change
- [ ] Bug fix
- [ ] New feature  
- [ ] Documentation update
- [ ] Refactoring
- [ ] Performance improvement

## Testing
- [ ] Compiled successfully
- [ ] Manual testing completed
- [ ] Existing functionality verified

## Screenshots (if applicable)
Add screenshots of terminal output or functionality.

## Additional Notes
Any additional information for reviewers.
```

### Review Process

1. At least one maintainer review required
2. All CI checks must pass
3. No merge conflicts
4. Documentation updated if needed

## Issue Reporting

### Bug Reports

Include the following information:
- Operating system and version
- GnuCOBOL version
- Steps to reproduce
- Expected vs actual behavior
- Error messages (if any)
- Sample data causing the issue

### Feature Requests

- Clear description of the feature
- Use case and benefits
- Possible implementation approach
- Any relevant examples or mockups

## Testing Guidelines

### Manual Testing Checklist

- [ ] Employee management (Add/Search/Update/Delete)
- [ ] Payroll calculation accuracy
- [ ] Report generation
- [ ] File I/O operations
- [ ] Authentication system
- [ ] Error handling

### Test Data

Use the provided sample data in `employee.dat` for testing. Create additional test cases for edge scenarios.

## Getting Help

- Check existing issues and documentation
- Ask questions in issue comments
- Contact maintainers: jsphere16@gmail.com
- Join discussions in GitHub Discussions

## Recognition

All contributors will be acknowledged in our README.md file and release notes.

---

## Quick Reference

### Useful Commands

```bash
# Compile main program
cobc -x -o payroll payroll.cbl

# Compile with debugging
cobc -x -g -o payroll payroll.cbl

# Check syntax only
cobc -fsyntax-only payroll.cbl

# Update from upstream
git fetch upstream
git checkout main
git merge upstream/main
```

### File Structure Reference

```
├── payroll.cbl          # Main program logic
├── employee.dat         # Employee data file  
├── payroll.dat          # Payroll transactions
├── reports.txt          # Generated reports
├── enhanced_modules.cbl # Additional modules
└── README.md           # Project documentation
```

Thank you for contributing to the Payroll Management System!
