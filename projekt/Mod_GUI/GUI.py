#!/usr/bin/python

from tkinter import *
import tkinter.ttk as ttk
import socket
import sys
from threading import Thread
from RecvThread import receiveThread
from Menu import Menues
import SendParser
import MiscUtils
import RecvParser
import ConnectionUtils
import TabUtils



class GUI(object):


    """

    Initiate the GUI

    master - The frame in which to display the GUI
    """
  
    def __init__(self,master):

        self.master = master
        
        Gstyle = ttk.Style()
        Gstyle.configure("TNotebook", background="#121A16", borderwidth=0)
        Gstyle.configure("TNotebook.Tab", background='#545854',foreground="black",borderwidth=1)
      
        self.nb = ttk.Notebook(master,style='TNotebook')
        self.nb.place(x=122, y=0)

        self.userWindow = Listbox(master, width=15,height=24)
        self.userWindow.config(background="#121A16",foreground="#00EB00",highlightthickness=0)
        self.userWindow.place(x=0,y=23)
        self.roomWindow = Listbox(master, width=15,height=24)
        self.roomWindow.config(background="#121A16",foreground="#00EB00",highlightthickness=0)
        self.roomWindow.place(x=689,y=23)
        self.roomLabel = Label(master,text="Available Rooms",font=("Helvetica",10))
        self.roomLabel.config(background="#121A16",foreground="#00EB00")
        self.roomLabel.place(x=695,y=0)
        self.userLabel = Label(master,text="Users",font=("Helvetica",10))
        self.userLabel.config(background="#121A16",foreground="#00EB00") 
        self.userLabel.place(x=30,y=0)
       
        self.temp = StringVar()

        self.message = Entry(master,width=70,textvariable = self.temp)
        self.message.config(background = "#121A16",foreground="#00EB00",insertbackground="#00EB00") 
        self.message.place(x=123,y=388)
        self.message.bind('<Return>',self.sendMessage)

        self.windowList = {}
        self.userList = {}
        self.rooms = {}
        self.configList = {}
        self.userList["global"] = ['']

        self.socketStatus = "disconnected"

        self.initiateConfig()

        self.currentTab = "global"
        self.nb.bind_all("<<NotebookTabChanged>>", lambda e: TabUtils.tabChangedEvent(e,gui))

        self.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        
        self.master.protocol('WM_DELETE_WINDOW', lambda: ConnectionUtils.closeClient(self))

        self.menues = Menues(self)

        TabUtils.addTab(self,"global")

#######################################################################################    

    def sendMessage(self,event):
        message = self.temp.get()
        SendParser.sendParser(self,message)
        
####################################################################################### 

    def Start(self):
        """

        Starts the background thread which continuously checks if there is a 
        new message from the server. 

        Also initiates the function checkQueue.
        """
        self.thread = receiveThread(self.serverSocket)
        self.thread.daemon = True
        self.thread.start()
        RecvParser.checkQueue(self)

####################################################################################### 

    def initiateConfig(self):
        """

        Reads all the information from the configurationfile. 
        """
        file = open('configFile','r')
        for line in file:
            self.parseConfig(line)

####################################################################################### 

    def parseConfig(self,configString):
        """

        interprets the read line.

        Keyword arguments:
        configString - The line to interpret.
        """
        index = configString.find("=")
        element = configString[0:index]
        message = configString[index+1:len(configString)-1]
        self.configList[element] = message  

#######################################################################################

    def initiateConnection(self):
        ConnectionUtils.connect(self)
        
####################################################################################### 

if __name__ == "__main__":
    """

    The main function.
    """
    root=Tk()
    root.geometry("810x408")
    root.configure(background="#121A16")
    root.title("Nuntii IRC")
    gui=GUI(root)
    gui.windowList["global"].config(state=NORMAL)
    gui.windowList["global"].insert(END,"Welcome "+gui.configList["userName"] +"!\n")
    gui.windowList["global"].insert(END,"----------------------------------------\n")
    gui.windowList["global"].config(state=DISABLED)
    if gui.configList["reconnectMode"] == 'auto':
        
        gui.initiateConnection()
    else:
        TabUtils.writeMessage(gui,"You are not connected to a server, use /connect IP") 
        
    root.mainloop()
