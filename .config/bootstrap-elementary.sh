#!/bin/sh
# Note: this script is not idempotent, and running it more than once will cause the manually added repositories
# (docker, code, spotify, etc) to be added to your /etc/apt/sources.list and /etc/apt/sources.list.d. You will notice
# warning messages when running any `apt` command telling you wher the duplicate repositories are defined. A temporary
# fix would be to comment the reported lines in /etc/apt/sources.list out. Making this script itempotent is something
# that will be worked on in the future.

installDocker() {
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"
    sudo curl -L "https://github.com/docker/compose/releases/download/1.23.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
    apt update && apt -y install docker-ce docker-ce-cli containerd.io
}

installCode() {
    add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
    apt update && apt -y install code
}

installAzureCli() {
    echo "deb [arch=amd64] https://packages.microsoft.com/repos/azure-cli/ bionic main" | \
        sudo tee /etc/apt/sources.list.d/azure-cli.list
    apt update && apt -y install azure-cli
}

installSpotify() {
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90
    add-apt-repository "deb http://repository.spotify.com stable non-free"
    apt update
    apt -y install spotify-client
}

installChrome() {
    curl -fsSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
    add-apt-repository "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main"
    apt update && apt -y install google-chrome-stable
}

installMerge() {
    curl -fsSL https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add -
    echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list
    apt update && apt -y install sublime-merge
}

installKubectl() {
    curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
    echo "deb https://apt.kubernetes.io/ kubernetes-xenial main" | tee -a /etc/apt/sources.list.d/kubernetes.list
    apt-get update && apt-get install -y kubectl
}

updateDock() {
    local plankLaunchers=$HOME/.config/plank/dock1/launchers
    updateWebBrowerDock
    command -v code > /dev/null && addCodeToDock
    command -v spotify > /dev/null && addSpotifyToDock
    removeUnusedProgramLaunchers
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

getMicrosoftGpgKey() {
    curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
    install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
    rm microsoft.gpg
}

apt -y install software-properties-common

ppas=$(cat $HOME/.config/ppas)
add-apt-repository -y $ppas
apt update

programs=$(cat $HOME/.config/programs)
removePrograms=$(cat $HOME/.config/remove)
apt -y install $programs && apt -y remove $removePrograms  # Only remove programs when replacements installed successfully

snaps=$(cat $HOME/.config/snaps)
snap install $snaps

getMicrosoftGpgKey # Needed for code and azure cli

# Adhoc installs; comment out lines for programs you do not want
installDocker
installCode
installSpotify
installChrome
installMerge
installAzureCli
installKubectl

# Add launchers to plank dock for elementary users
[ $(lsb_release -is) = "elementary" ] && updateDock

# Comment this line out if you do not want to use fish as your default shell
command -v fish > /dev/null && chsh -s $(which fish)
