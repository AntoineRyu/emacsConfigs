###############
### Aliases ###
###############

# Non mujin stuff
alias ll='ls -lart'
#alias remacs='emacs -Q -l ~/emacsConfigs/init.el'
alias kk='kill -9 %'
alias gka='gitk --all'

gkas(){
    gitk --all --select-commit="$1"
}

# Mujin stuff
alias plani="mujin_planningserver_startbackendmaster.py --logdir '' --loglevel 'info'"
alias pland="mujin_planningserver_startbackendmaster.py --logdir '' --loglevel 'debug'"
alias ui="mujin_teachworkerui_start.py --usermode dev"
alias loadscene="mujin_planningcommon_loadscene.py"

vnc(){
    vncviewer controller"$1"
}

#######################
### Auto completers ###
#######################

# our handler that returns choices by populating Bash array COMPREPLY
# (filtered by the currently entered word ($2) via compgen builtin)
_gitbranch_complete() {
    branches=$(git branch -l | cut -c3-)
    COMPREPLY=($(compgen -W "$branches" -- "$2"))
}

# Jhbuild names autocomplete
_jhbuild_complete()
{
	cur=${COMP_WORDS[COMP_CWORD]}
    command_list="`jhbuild list -a`"	
	for i in $command_list; do
		if [ -z "${i/$cur*}" ]; then
			COMPREPLY=( ${COMPREPLY[@]} $i )
		fi
	done
}

#########################
### Utility functions ###
#########################

# jhbuild and rsync 
rsyncrepo(){
    if [ $# -eq 0 ]; then
        echo "Usage: rsyncrepo TARGET_ADDRESS JHBUILD_REPO"
        return
    elif [ $# -eq 1 ]; then
        echo "Please provide a jhbuild repository to rsync to"
        return
    fi
    
    if [[ "$1" =~ ^[0-9]+$ ]]; then
        REMOTE="c+controller"$1
        echo "rsync to REMOTE="$REMOTE
    elif [[ "$1" =~ ^\.[0-9]+$ ]]; then
        REMOTE=172.17.0$1
        echo "rsync to REMOTE="$REMOTE
    else
        REMOTE=$1
        echo "rsync to REMOTE="$REMOTE
    fi
    path=$(pwd)
    cd $MUJINJH_APPTEACHWORKER_HOME/docker
    make BUILD=release PRESET=ecs8xxx JOBS=4 jhbuild-run rsync BUILD_OPTS="--no-network" CMD="jhbuild buildone -n "$2"" REMOTE=$REMOTE MODULES="$2" DEBUG=yes
    cd $path
}
complete -F _jhbuild_complete rsyncrepo

# Replace a jhbuild repository with a target one. Meant only for testing environment! Completely overwrites history of the replaced repository with the other!
replacerepo(){
    if [ $# -eq 0 ]; then
        echo "Usage: replacerepo REPLACE_REPO WITH_THIS_REPO"
        return
    elif [ $# -eq 1 ]; then
        echo "Please provide a target repository to copy"
        return
    fi
    
    cd $MUJINJH_APPTEACHWORKER_HOME
    git fetch
    git checkout "$1"
    git reset --hard origin/"$2"
    git push origin HEAD -f
    git submodule update --init --recursive
    git submodule foreach --recursive "git push origin HEAD:'$1' -f"
    git submodule foreach --recursive "git checkout '$1'"
    git submodule foreach --recursive "git reset --hard origin/'$1'"
}
complete -F _gitbranch_complete replacerepo

advancerepo(){
    git submodule foreach --recursive "git checkout '$1'"
    git submodule foreach --recursive "git reset --hard origin/'$1'"
    branch_name=$(git symbolic-ref HEAD 2>/dev/null | cut -d"/" -f 3)
    git branch --set-upstream-to=origin/$branch_name $branch_name
    if mujin_jhbuildcommon_advancemodule.bash "$1"; then
        path=$(pwd)
        cd $MUJINJH_APPTEACHWORKER_HOME
        mujin_jhbuildcommon_advancesubmodules.bash "$1"
        cd $path
    else
        # Retry
        git submodule foreach --recursive "git checkout '$1'"
        git submodule foreach --recursive "git reset --hard origin/'$1'"
        if mujin_jhbuildcommon_advancemodule.bash "$1"; then
            path=$(pwd)
            cd $MUJINJH_APPTEACHWORKER_HOME
            mujin_jhbuildcommon_advancesubmodules.bash "$1"
            cd $path
        fi
    fi
}
complete -F _gitbranch_complete advancerepo

diffbranches() {
    git checkout "$1"
    mujin_jhbuildcommon_initjhbuild.bash
    cp -r modulesets/ /tmp/"$1"
    git checkout "$2"
    mujin_jhbuildcommon_initjhbuild.bash
    diff -r modulesets/ /tmp/"$1"
}
complete -F _jhbuild_complete diffbranches
