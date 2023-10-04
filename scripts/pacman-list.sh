#!/usr/bin/env bash

pacman -Qem > /home/pk/pacman-aur.txt 2>/dev/null
pacman -Qen > /home/pk/pacman.txt 2>/dev/null
