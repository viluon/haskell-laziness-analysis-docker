FROM archlinux:20200205 AS arch-ghc-build-env

RUN pacman -Sy --noconfirm archlinux-keyring
RUN rm -rf /etc/pacman.d/gnupg; pacman-key --init; pacman-key --populate archlinux
RUN pacman -Su --noconfirm
RUN pacman -S --noconfirm reflector
RUN sh -c 'reflector --score 20 -f 8 --sort rate > /etc/pacman.d/mirrorlist'
RUN pacman -S --needed --noconfirm git base-devel go python python-sphinx libedit numactl exa

# use all possible cores for subsequent package builds
RUN sed -i 's,#MAKEFLAGS="-j2",MAKEFLAGS="-j$(nproc)",g' /etc/makepkg.conf
# don't compress the packages built here
RUN sed -i "s,PKGEXT='.pkg.tar.xz',PKGEXT='.pkg.tar',g" /etc/makepkg.conf
# set up the packager user
RUN useradd --create-home packager
COPY bashrc.sh /home/packager/.bashrc
COPY packager-actions /etc/sudoers.d/

USER packager
WORKDIR /home/packager/
RUN git clone https://aur.archlinux.org/yay.git
WORKDIR /home/packager/yay/
RUN makepkg -i --noconfirm

WORKDIR /home/packager/
RUN mkdir -p .ghcup/bin/
RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o .ghcup/bin/ghcup
RUN chmod +x .ghcup/bin/ghcup
USER root
RUN curl -sSL https://get.haskellstack.org/ | sh
USER packager

RUN .ghcup/bin/ghcup install ghc 8.6.5
RUN .ghcup/bin/ghcup set 8.6.5
RUN .ghcup/bin/ghcup install cabal


USER root
RUN ln -s /home/packager/.ghcup/bin/ghc     /usr/sbin/ghc
RUN ln -s /home/packager/.ghcup/bin/ghc-pkg /usr/sbin/ghc-pkg
RUN stack install --system-ghc alex happy
RUN ln -s /root/.local/bin/alex             /usr/sbin/alex
RUN ln -s /root/.local/bin/happy            /usr/sbin/happy
RUN chmod a+x  /root/
RUN chmod a+x  /root/.local/
RUN chmod a+x  /root/.local/bin/
RUN chmod a+rx /root/.local/bin/alex
RUN chmod a+rx /root/.local/bin/happy


# build GHC
FROM arch-ghc-build-env AS arch-ghc-cleanbuild
USER packager
WORKDIR /home/packager/
RUN source /home/packager/.bashrc && cabal update
