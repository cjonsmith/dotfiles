#!/bin/sh
installDocker() {
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"
    apt update && apt -y install docker-ce docker-ce-cli containerd.io
}

installCode() {
    curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
    install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
    add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
    apt update && apt -y install code
}

installSpotify() {
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90
    add-apt-repository "deb http://repository.spotify.com stable non-free"
    apt update
    apt -y install spotify-client
}

updateDock() {
    local plankLaunchers=$HOME/.config/plank/dock1/launchers
    updateWebBrowerDock
    command -v code > /dev/null && addCodeToDock
    command -v spotify > /dev/null && addSpotifyToDock
    removeUnusedPrograms
}

updateWebBrowerDock() {
    local epiphanyLauncher=org.gnome.Epiphany.dockitem
    local firefoxLauncher=firefox.dockitem
    command -v epiphany || [ -f $plankLaunchers/$epiphanyLauncher ] && rm $plankLaunchers/$epiphanyLauncher
    command -v firefox > /dev/null && \
        printf "[PlankDockItemPreferences]\nLauncher=file:///usr/share/applications/firefox.desktop\n" > $plankLaunchers/$firefoxLauncher 
}

addCodeToDock() {
    local codeLauncher=code.dockitem
    printf "[PlankDockItemPreferences]\nLauncher=file:///usr/share/applications/code.desktop\n" > $plankLaunchers/$codeLauncher
}

addSpotifyToDock() {
    local spotifyLauncher=spotify.dockitem
    printf "[PlankDockItemPreferences]\nLauncher=file:///usr/share/applications/spotify.desktop\n" > $plankLaunchers/$spotifyLauncher
}

removeUnusedProgramLaunchers() {
    rm $plankLaunchers/io.elementary.music.dockitem
    rm $plankLaunchers/io.elementary.videos.dockitem
    rm $plankLaunchers/io.elementary.photos.dockitem
}

apt -y install software-properties-common

ppas=$(cat $HOME/.config/ppas)
add-apt-repository -y $ppas
apt update

programs=$(cat $HOME/.config/programs)
removePrograms=$(cat $HOME/.config/remove)
apt -y install $programs && apt -y remove $removePrograms  # Only remove programs when replacements installed successfully

# Adhoc installs; comment out lines for programs you do not want
installDocker
installCode
installSpotify

# Add launchers to plank dock for elementary users
[ $(lsb_release -is) = "elementary" ] && updateDock

# Comment this line out if you do not want to use fish as your default shell
command -v fish > /dev/null && chsh -s $(which fish)