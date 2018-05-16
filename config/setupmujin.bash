#!/bin/bash
# Setup mujin environment for support laptops

sudo apt-get install daemon

if [ ! -f "$MUJIN_PRIVATE_DIR"/mujin_conf.bash ]; then
    mujin_controllercommon_generatemujinconf.py
fi

sudo sed -i 's/\(export MUJIN_USE="\).*/\1controllercommon_rsyslogd controllercommon_logrotate webstack planningserver itlprocess"/' "$MUJIN_PRIVATE_DIR"/mujin_conf.bash

MUJIN_CONF='source $HOME/mujin_conf.bash'
MUJIN_COMMON='source $HOME/mujin/jhbuildcommon/setup.bash'
MUJIN_TEACH='source $HOME/mujin/jhbuildappteachworker/setuptestdev.bash'
MUJIN_PICK='source $HOME/mujin/jhbuildapppickworker/setuptestdev.bash'
grep -q -F "$MUJIN_CONF" ~/.bashrc || echo "$MUJIN_CONF" >> ~/.bashrc
grep -q -F "$MUJIN_COMMON" ~/.bashrc || echo "$MUJIN_COMMON" >> ~/.bashrc
grep -q -F "$MUJIN_TEACH" ~/.bashrc || echo "$MUJIN_TEACH" >> ~/.bashrc
grep -q -F "$MUJIN_PICK" ~/.bashrc || echo "" >> ~/.bashrc
sudo sed -i '/jhbuildapppickworker/d' ~/.bashrc

mujin_jhbuildcommon_updatejhbuildcommon.bash

git@git.mujin.co.jp:jhbuild/jhbuildcommon.git
URL=git@git.mujin.co.jp:jhbuild/jhbuildplanningcommon.git
if [ ! -d "$MUJINJH_APPTEACHWORKER_HOME" ] ; then
    git clone $URL "$MUJINJH_APPTEACHWORKER_HOME"
else
    cd "$MUJINJH_APPTEACHWORKER_HOME"
    git pull $URL
fi

cd $MUJINJH_APPTEACHWORKER_HOME
cp -v .jhbuildrc ~/.jhbuildrc
git checkout antoine

mujin_jhbuildcommon_initjhbuild.bash
jhbuild -f .jhbuildrc sysdeps --install
jhbuild

sudo service mujin start
# Use "mujin_webstack_listldapusers.bash" to list the users, "sudo which mujin_env; mujin_webstack_adduser.bash mujin mujin" to add a new user

