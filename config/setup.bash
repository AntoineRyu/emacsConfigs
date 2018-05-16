#!/bin/bash
# for emacs, terminator and other non mujin stuff

sudo apt-get install gitk silversearcher-ag bzr
sudo apt-get install texinfo virtualenv
#sudo pip install

# My emacs setup
git clone https://github.com/AntoineRyu/emacsConfigs.git $HOME

# Terminator from source
bzr branch lp:terminator $HOME
cd $HOME/terminator
./setup.py install --record=install-files.txt

# Copy configs
cp $HOME/emacsConfigs/configs/.bash_aliases $HOME
cp $HOME/emacsConfigs/configs/.gitconfig $HOME
cp $HOME/emacsConfigs/configs/flake8 $HOME/.config
# Terminator configs?
