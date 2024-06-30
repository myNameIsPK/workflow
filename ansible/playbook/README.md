# Ansible for dotfiles

## Install multiple profiles
**Example**: Arch Laptop
```yaml
# playbook/arch_laptop.yaml
---
- name: Check packages
  hosts: all
  remote_user: pk
  vars_files:
    - ../vars/arch_laptop.yaml
  vars:
    debug_enabled: false
    enable_profiles:
      - core
      - xorg
  roles:
    - pacman
    - aur
    - systemd
```
- run playbook on localhost
```console
$ ansible-playbook localhost playbook/arch_laptop.yaml
```
- run playbook on remote (set ~/.ssh/config and add host in inventory/arch_laptop.yaml)
```console
$ ansible-playbook -i inventory/arch_laptop.yaml playbook/arch_laptop.yaml
```
- Each role has their tags, so use it to filter un want operations
```console
$ ansible-playbook localhost playbook/arch_laptop.yaml -t pacman # pacman only
$ ansible-playbook localhost playbook/arch_laptop.yaml --skip-tags systemd # skip systemd
```
<!--TODO: use extras vars to overwrite `enable_profiles` -->
- Use `--extra-vars` to overwrite profiles.
```console
$ ansible-playbook localhost playbook/arch_laptop.yaml -e "enable_profiles=['gpu','gpu-util']" -e "debug_enabled=true"
```
> [!TIP]
> use "debug_enabled=" to force disable debug.
