[Blog Hub](../index) | [Previous](post9)

# Post 10<br>A Functioning Functional GUI

## Why Not Haskell?

<br>

We've worked a lot with Haskell and we even have a cool project with Roman Numerals built in it, so why aren't we going to use Haskell for making a visual program? Because usability and support for UI packages in Haskell is **_seriously_** lacking. It was difficult enough to get any of the packages running, but as a Windows user, I wasn't happy with the feasability of any of the environments I set up for the purposes of this blog. Luckily, F# has well supported and brain-dead easy to set up UI packages! So, we're going to be using [Avalonia FuncUI](https://github.com/AvaloniaCommunity/Avalonia.FuncUI/) for this little tour of _Functioning Functional GUIs_.

---

## Setting Up Avalonia

<br>

Like I said, setting this up is very simple, just make sure you have [Visual Studio](https://visualstudio.microsoft.com/downloads/) or [Visual Studio Code](https://code.visualstudio.com/download) installed on your computer before hand.

To get started and download some template projects, simply run the following command in your terminal or power shell:

    dotnet new --install JaggerJo.Avalonia.FuncUI.Templates

After that finishes up, you can then make a new project from a template by running one of the following commands:

    dotnet new funcui.basic -n NewApp

<br>

    dotnet new funcui.full -n NewApp

<br>

    dotnet new funcui.quickstart -n NewApp

Of course, just change the name "NewApp" to whatever you would like to name the app. To open it in vscode, simply cd into the new directory made called NewApp (or whatever you named the project) then enter the command `code .` to open up a new isntance of vscode with the project ready to go.

    > dotnet new funcui.basic -n NewApp

    > cd ./NewApp
      -- OR --
    > cd .\NewApp

    > code .

Then to run the app simply enter the following command while inside the project directory.

    dotnet run

A new window should pop up that looks something like this:

<img src="../Files/fhsarp/Images/NewAppTemplate.png">

[Reference](https://github.com/AvaloniaCommunity/Avalonia.FuncUI/)

[Blog Hub](../index) | [Previous](post9)
