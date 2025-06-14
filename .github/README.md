# CI/CD Documentation

This directory contains GitHub Actions workflows for automated testing and maintenance of the dotfiles repository.

## Workflows

### 🔍 Core Validation (`ci.yml`)
**Triggers**: Push, PR, manual
- **Flake Check**: Validates flake syntax and structure
- **Build Testing**: Tests all NixOS and Darwin configurations  
- **Formatting**: Ensures consistent code style with nixfmt
- **Dead Code Detection**: Finds unused Nix expressions with deadnix
- **Static Analysis**: Linting and best practices with statix
- **Dependency Analysis**: Tracks flake inputs and updates

### 🔒 Security Checks (`security.yml`)
**Triggers**: Push, PR, weekly schedule, manual
- **Secret Scanning**: Detects potential secrets in configuration files
- **Network Security**: Checks for hardcoded IPs and insecure settings
- **SSH Configuration**: Validates SSH security settings
- **Cryptographic Settings**: Identifies weak crypto configurations  
- **File Permissions**: Ensures proper file permissions
- **Vulnerability Scanning**: Basic dependency vulnerability checks

### 📦 Dependency Updates (`update-inputs.yml`)
**Triggers**: Weekly schedule (Mondays 9 AM UTC), manual
- **Automated Updates**: Updates all flake inputs weekly
- **Selective Updates**: Manual trigger supports updating specific inputs
- **Testing**: Validates updated configurations build successfully
- **Pull Requests**: Automatically creates PRs with update summaries
- **Change Detection**: Only creates PRs when changes are detected

### 📚 Documentation (`documentation.yml`)
**Triggers**: Push, PR, manual
- **Auto-Generated Docs**: Creates documentation from configuration
- **Module Documentation**: Documents available modules and options
- **Host Documentation**: Documents host-specific configurations
- **GitHub Pages**: Publishes documentation to GitHub Pages
- **README Validation**: Ensures proper documentation structure

### ⚡ Performance Analysis (`performance.yml`)
**Triggers**: Push, PR, weekly schedule, manual
- **Build Time Tracking**: Measures configuration build times
- **Cache Analysis**: Evaluates Nix cache effectiveness
- **Dependency Metrics**: Tracks package counts and complexity
- **Performance Reports**: Generates detailed performance insights
- **PR Comments**: Adds performance analysis to pull requests

## Setup Instructions

### 1. Repository Secrets (Optional)
Add these secrets in your GitHub repository settings for enhanced functionality:

```
CACHIX_AUTH_TOKEN - For Cachix binary cache (optional but recommended)
```

### 2. Enable GitHub Pages
1. Go to repository Settings → Pages
2. Set Source to "GitHub Actions"
3. Documentation will be available at `https://yourusername.github.io/dotfiles`

### 3. Configure Dependabot
The included `dependabot.yml` automatically:
- Updates GitHub Actions weekly
- Creates PRs for action updates
- Assigns PRs to you for review

## Local Testing

Run the local CI script before pushing:

```bash
./scripts/ci-local.sh
```

This script runs the same checks as the CI pipeline locally.

## Workflow Customization

### Adding New Hosts
Add new host names to the `matrix.host` arrays in `ci.yml`:

```yaml
matrix:
  host: [hyperion, helios, atlas, janus, pallas, nike, nexus, hestia, your-new-host]
```

### Modifying Update Schedule
Change the cron schedule in `update-inputs.yml`:

```yaml
schedule:
  - cron: '0 9 * * 1'  # Every Monday at 9 AM UTC
```

### Customizing Performance Tests
Modify the `configs_to_test` array in `performance.yml`:

```yaml
configs_to_test=("hyperion" "eos" "your-config")
```

## Workflow Status

You can monitor workflow status with badges in your README:

```markdown
[![CI](https://github.com/yourusername/dotfiles/workflows/CI/badge.svg)](https://github.com/yourusername/dotfiles/actions)
[![Security](https://github.com/yourusername/dotfiles/workflows/Security%20Checks/badge.svg)](https://github.com/yourusername/dotfiles/actions)
```

## Best Practices

### Commit Messages
Use conventional commit format for better automation:
- `feat:` for new features
- `fix:` for bug fixes  
- `chore:` for maintenance
- `docs:` for documentation
- `ci:` for CI/CD changes

### PR Reviews
- All PRs require CI to pass
- Security checks must pass
- Performance regressions are flagged
- Documentation is auto-updated

### Branch Protection
Consider enabling branch protection rules:
- Require PR reviews
- Require status checks to pass
- Require branches to be up to date
- Restrict pushes to main branch

## Troubleshooting

### Common Issues

**Build Failures**: 
- Check flake syntax with `nix flake check`
- Verify host configurations exist
- Ensure platform compatibility

**Permission Errors**:
- Verify GitHub token permissions
- Check repository settings
- Ensure workflows have write access

**Cache Issues**:
- Clear local cache: `nix store gc`  
- Check Cachix configuration
- Verify binary cache accessibility

**Documentation Failures**:
- Ensure GitHub Pages is enabled
- Check file permissions
- Verify markdown syntax

### Getting Help

1. Check workflow logs in GitHub Actions tab
2. Run local CI script for debugging
3. Verify flake configuration locally
4. Check Nix documentation for syntax issues

## Contributing

When adding new workflows:
1. Test locally first
2. Document new features
3. Update this README
4. Follow existing patterns
5. Add appropriate error handling