# Aliases

alias ll='ls -lart'
alias remacs='emacs -Q -l ~/emacsConfigs/init.el'
alias kk='kill -9 %'
alias gka='gitk --all'
alias plani="mujin_planningserver_startbackendmaster.py --logdir '' --loglevel 'info'"
alias pland="mujin_planningserver_startbackendmaster.py --logdir '' --loglevel 'debug'"
alias ui="mujin_teachworkerui_start.py --usermode dev"
alias emax='emacs -Q -l ~/dotemacs/init.el'

# Functions
rsyncrobotbridge(){
    if ! [[ "$1" =~ ^[0-9]+$ ]]; then
        echo "Require to pass the number of a remote controller" 1>&2
        return 1
    fi

    path=$(pwd)
    cd $MUJINJH_APPTEACHWORKER_HOME/docker
    make BUILD=release PRESET=ecs8xxx JOBS=4 jhbuild-run rsync BUILD_OPTS="--no-network" CMD="jhbuild buildone -n mujinrobotbridgescpp" REMOTE=c+controller"$1" MODULES=mujinrobotbridgescpp DEBUG=yes
    cd $path
}

replaceantoine(){
    cd $MUJINJH_APPTEACHWORKER_HOME
    git cc antoine
    git reset --hard origin/tw2019
    git push origin HEAD -f
    git submodule foreach --recursive git checkout antoine
    git submodule foreach --recursive git reset --hard origin/tw2019
    git submodule foreach --recursive git push origin HEAD -f
    mujin_jhbuildcommon_advancesubmodules.bash antoine
}

diffbranches() {
    git checkout "$1"
    mujin_jhbuildcommon_initjhbuild.bash
    cp -r modulesets/ /tmp/"$1"
    git checkout "$2"
    mujin_jhbuildcommon_initjhbuild.bash
    diff -r modulesets/ /tmp/"$1"
}
