#!/bin/bash
# for emacs, terminator and other non mujin stuff

sudo apt-get install gitk silversearcher-ag bzr
sudo apt-get install texinfo virtualenv
#sudo pip install

# My emacs setup
git clone https://github.com/AntoineRyu/emacsConfigs.git $HOME/emacsConfigs

# Terminator from source
bzr branch lp:terminator $HOME/terminator
cd $HOME/terminator
sudo ./setup.py install --record=install-files.txt

# Copy configs
cp $HOME/emacsConfigs/config/.bash_aliases $HOME
cp $HOME/emacsConfigs/config/.gitconfig $HOME
cp $HOME/emacsConfigs/config/flake8 $HOME/.config
# Terminator configs? -> for shift+ctrl+alt+a bug
