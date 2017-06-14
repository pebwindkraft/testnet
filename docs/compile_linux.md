First you have to install the dependencies. This depends on your Linux distribution. If unsure you can try `lsb_release -d` to check your distribution.

## For Ubuntu

Make sure that that you are running Ubuntu 16 or later:

(earlier versions of ubuntu requires manually installing the latest version of erlang, because the package manager installs an old version)

Use this command to check your version number
```
lsb_release -a
```

Make sure that your system is up-to-date:
```
sudo apt-get update
```
and
```
sudo apt-get upgrade
```

For Ubuntu, install following dependencies:

```
sudo apt-get install erlang libncurses5-dev libssl-dev unixodbc-dev g++ git erlang-base-hipe
```

## For Arch

Make sure your system is up to date. This step is important, becouse it also synchronises repository database:

```
sudo pacman -Syu
```

Install the dependencies:

```
pacman -S --needed community/erlang extra/git extra/unixodbc core/gcc core/ncurses extra/wget
```

## After Dependencies

Next, download Aeternity Testnet. Optionally you can run next steps with a non-root user, for better security.

```
git clone https://github.com/aeternity/testnet.git
```
Now you can go into the directory, and compile the aeternity testnet.

```
cd testnet/
sh install.sh
```

Now you can run your node. with ```sh start.sh```
