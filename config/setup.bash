#!/bin/bash

sudo apt-get install gitk silversearcher-ag libterm-readkey-perl git-lfs
sudo apt-get install texinfo virtualenv
pip install python-lsp-server black black-macchiato isort

# fzf - Command line fuzzy finder
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install --completion --key-bindings --update-rc

# Terminator - terminal
git clone https://github.com/gnome-terminator/terminator.git $HOME/terminator
cd $HOME/terminator
python3 setup.py build
python3 setup.py install --single-version-externally-managed --record=install-files.txt

# Copy configs
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cp $SCRIPTPATH/.bash_aliases $HOME
cp $SCRIPTPATH/.gitconfig $HOME
cp $SCRIPTPATH/flake8 $HOME/.config
cp $SCRIPTPATH/setup.cfg $HOME/.config
cp $SCRIPTPATH/gitk $HOME/.config/git/
cp $SCRIPTPATH/ssh_config $HOME/.ssh/config
