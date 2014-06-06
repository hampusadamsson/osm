#!/usr/bin/python

from tkinter import *
import tkinter.ttk as ttk
import tkinter
import socket
from tkinter import *
import time
import sys
from threading import Thread
import threading
from RecvThread import StoppableThread
import errno
import os
from time import sleep
from Connect import connectToServer
from menu import UserMenu
from menu import RoomMenu
import copy



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
        self.roomLabel = Label(master,text="Tillgangliga Rum",font=("Helvetica",10))
        self.roomLabel.config(background="#121A16",foreground="#00EB00")
        self.roomLabel.place(x=695,y=0)
        self.userLabel = Label(master,text="Anvandare",font=("Helvetica",10))
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
        
        self.configList["userName"] = ""

        self.socketStatus = "disconnected"
        self.config = ()

        self.currentTab = "global"
        self.nb.bind_all("<<NotebookTabChanged>>", self.tabChangedEvent)

        self.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.master.protocol('WM_DELETE_WINDOW', self.closeConnection)

        self.globalRoom = Text(master,state=DISABLED)
        self.globalRoom.config(background = "#121A16",foreground="#00EB00")
        self.nb.add(self.globalRoom, text='global')
        self.windowList["global"] = self.globalRoom

    
        
    def addTab(self,name):

        """

        Creates a new tab and corrensponding chat window and stores them

        Keyword arguments:
        name - The name of the tab
        """    
        tab = Text(self.master,state=DISABLED)
        tab.config(background = "#121A16",foreground="#00EB00")
        self.windowList[name] = tab
        self.userMenu.createRoomMenu(self.windowList)
        self.nb.add(tab, text=name)
        self.userMenu.setRoomList(self.windowList)
        self.roomMenu.setRoomList(self.windowList)

   

    def GetTime(self):
        """

        Import the current time and return it
        """
        return "<" + time.strftime("%H:%M")+" "

    def sendMessage(self,event):
        """

        Retrieves the text from the entry field, parses it and sends it to 
        the server in a bytearray form. 

        Keyword arguments: 
        event - The return-key was pressent

        Side-effects:
        Checks if the command is valid. If so, sends it to the server
        """
        mtext1 = self.temp.get()
        argumentString = self.messageSplit(mtext1)

        if (argumentString[0] == "/connect"):
            if(self.socketStatus == "disconnected"):
               self.configList["ipAdress"] = argumentString[1]
               self.reconnect()
            else:
                if argumentString[1] == self.configList["ipAdress"]:
                    self.writeMessage("Du ar redan ansluten till " +argumentString[1],"syscall")
                else:
                    self.disconnect()
                    self.configList["ipAdress"] = argumentString[1]
                    self.reconnect()
                self.message.delete(0,END)
            
                    
        elif (self.socketStatus != "ok"):
            self.writeMessage("Du ar inte ansluten till en server, anslut med /connect IP","syscall")
            self.message.delete(0,END)
            
        elif (argumentString[0] == "/join"):
            success = 0
            if (" " in argumentString[1]):
                argumentString2 = self.messageSplit(argumentString[1])
                if self.noDuplicate(argumentString2[0]):
                   success = 1
                arguments = 2
            else:
                if self.noDuplicate(argumentString[1]):
                    success = 1
                arguments = 1
            if success == 1:
                if (arguments == 2):
                    msg_temp = argumentString2[0] + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')   
                else:
                    msg_temp = argumentString[1] + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')
                self.serverSocket.send(msg)
                self.message.delete(0,END)
            else:
                self.writeMessage("Du ar redan med i det angivna rummet!","syscall")
                self.message.delete(0,END)

        elif (argumentString[0] == "/invite"):
            msg_temp =self.currentTab + " " + mtext1+'\n'
            msg = msg_temp.encode('UTF-8')
            self.serverSocket.send(msg)
            self.message.delete(0,END)
        elif (argumentString[0] == "/clea"):
            self.clearWindow()
            self.message.delete(0,END)

        elif (argumentString[0] == "/rename"):
            
            msg_temp =self.currentTab + " " + mtext1+'\n'
            msg = msg_temp.encode('UTF-8')
            self.serverSocket.send(msg)
            self.message.delete(0,END)
            
        elif (argumentString[0] == "/exit"):
            if argumentString[1] == "global":
                self.writeMessage("Du kan inte go ur global!","syscall")
                self.message.delete(0,END)
            else:
                if(not self.noDuplicate(argumentString[1])):
                    self.deleteTab(argumentString[1])
                    self.windowList.pop(argumentString[1],None)
                    self.userMenu.createRoomMenu(self.windowList)
                    msg_temp ="global" + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')
                    self.serverSocket.send(msg)
                    self.message.delete(0,END)
                else:
                    self.writeMessage("Du ar inte inne i rummet: " + argumentString[1]+"!","syscall")
                    self.message.delete(0,END)
        else:
            mtext = self.currentTab + " " + mtext1+'\n'
            msg = mtext.encode('UTF-8')
            self.serverSocket.send(msg)
            self.message.delete(0,END)

    def closeConnection(self):
        """

        Shuts down the connection and window when the user closes the window
        """
        if self.socketStatus == "ok":            
            self.serverSocket.shutdown(socket.SHUT_RDWR)
        self.serverSocket.close()
        self.master.destroy()
        sys.exit(0)    

    def Start(self):
        """

        Starts the background thread which continuously checks if there is a 
        new message from the server. 

        Also initiates the function checkQueue.
        """
        self.thread = StoppableThread(self.serverSocket)
        self.thread.daemon = True
        self.thread.start()
        self.checkQueue()
        self.initiateMenues()
        self.userMenu.setCurrent(self.configList["userName"])
        self.roomMenu.setCurrent("global")
        
        

   


    def initiateMenues(self):
        """

        Creates userMenu and roomMenu that appears when the user right-clicks.
        """
        self.userMenu = UserMenu(self.master,self.serverSocket,self.configList["userName"])
        self.userWindow.bind('<Button-3>',lambda e: self.setAndPopup(e,self.userWindow))
        self.userMenu.createRoomMenu(self.windowList)

        self.roomMenu = RoomMenu(self.master,self.serverSocket)
        self.roomWindow.bind('<Button-3>',lambda e: self.setAndPopup(e,self.roomWindow))

    

    def setAndPopup(self,event,listbox):
        """
        Sets the listbox item the user right clicks on to active and displays 
        the corresponding menu.

        Keyword arguments:
        event - The user right clicks
        listbox - The listbox on which the user right clicked 
        """
        listbox.activate(listbox.nearest(event.y))
        if (listbox == self.userWindow):
            self.userMenu.setCurrent(listbox.get(ACTIVE))
            self.userMenu.popup(event)
        else:
            self.roomMenu.setCurrent(listbox.get(ACTIVE))
            self.roomMenu.popup(event)

   
    def checkQueue(self):
        """

        Parses the answer from the server and performes the corresponding 
        command.

        Side effects: 

        Can be one of the following: A new tab is created, 
        the username is changed, a message is printed in the current window.
        """
        
        stopSign = 1
        respons = self.thread.returnQueue()
        if (respons == "empty"):
            pass
        elif(respons == "Disconnected"):
            self.socketStatus = "disconnected"
            self.disconnect()
            stopSign = 0
            if self.configList["reconnectMode"] == 'auto':
                self.writeMessage("Tappade anslutningen, forsoker ateransluta automatiskt","syscall")
                self.reconnect()
            else:
                self.writeMessage("Tappade anslutningen till servern, anslut manuellt med /connect IP","syscall")           
        elif(respons[0][0] == "{"):
                temp = respons[1:len(respons)-2]
                if " " in temp:
                    commandString = self.messageSplit(temp)
                    if commandString[0] == 'success':
                        commandString2 = self.messageSplit(commandString[1])
                        self.rooms[commandString2[0]] = [commandString[0],commandString2[1]]
                        self.addTab(commandString2[0])
                    elif commandString[0] == 'error':
                        self.writeMessage("Rummet ar slutet, du maste bli inbjuden","syscall")
                    elif (commandString[0] == 'invited'):
                        if(self.noDuplicate(commandString[1])):
                            self.addTab(commandString[1])

                    elif commandString[0] == 'whois':
                        whoisInfo = commandString[1].split(",")
                        self.writeMessage("","syscall")
                        self.writeMessage("------------------------","syscall")
                        for element in whoisInfo:
                            self.writeMessage(element,"syscall")
                        self.writeMessage("------------------------","syscall")
                        self.writeMessage("","syscall")

                    elif commandString[0] == 'track':
                        trackList = commandString[1].split(",")
                        self.writeMessage("","syscall")
                        self.writeMessage("-------------------------------------","syscall")
                        self.writeMessage("Anvandaren ar med i foljande rum:","syscall")
                        for element in trackList:
                            self.writeMessage(element,"syscall")
                        self.writeMessage("-------------------------------------","syscall")
                        self.writeMessage("","syscall")
                    elif commandString[0] == 'username':
                        self.configList["userName"] = commandString[1]
                        self.userMenu.rename(commandString[1])
                        self.rename()
                        
                    else:
                        self.userList[commandString[0]] = commandString[1].split(",")
                        if (commandString[0] == self.currentTab):
                            self.fillUserList(self.currentTab)
                else:
                    roomList = temp.split(",")
                    self.roomWindow.delete(0,END)
                    for room in roomList:    
                        self.roomWindow.insert(END,room)
        else:
            argumentString = self.messageSplit(respons)          
            self.windowList[argumentString[0]].config(state=NORMAL)
            self.windowList[argumentString[0]].insert(END,self.GetTime() + argumentString[1])
            self.windowList[argumentString[0]].yview(END)
            self.windowList[argumentString[0]].config(state=DISABLED)
        if stopSign == 1:
            self.master.after(50,self.checkQueue)

    


    def fillUserList(self,roomName):
        """

        Fill the userList with the usernames in the current room.

        Keyword arguments:
        roomName - In which to list the users.
        """
        self.userWindow.delete(0,END)
        for userName in self.userList[roomName]:     
            self.userWindow.insert(END,userName)

    def welcome(self):
        """

        The welcome message the user is presented with.
        """
        self.globalRoom.config(state=NORMAL)
        self.globalRoom.insert(END,"Valkommen "+self.configList["userName"] +"!\n")
        self.globalRoom.insert(END,"----------------------------------------\n")
        self.globalRoom.config(state=DISABLED)

    def sendUserName(self):
        """

        Sends the userName to the server. 
        """
        userName = self.configList["userName"]
        temp = userName+'\n'
        msg = temp.encode('UTF-8')
        self.serverSocket.send(msg)

    def tabChangedEvent(self,event):
        """

        Changes to the current tab.

        Keyword arguments:
        event - User clicks on a new tab.
        """
        self.currentTab = event.widget.tab(event.widget.index("current"),"text")
        self.fillUserList(self.currentTab)
        
    def messageSplit(self,input):
        """

        Reads to a blankspace and splits the string into two.

        Keyword arguments:
        input - The string to split.
        """
        index = input.find(" ")
        message = (input[0:index],input[index+1:len(input)])
        return message

    

    def deleteTab(self,name):
        """

        Deletes the selected tab.

        Keyword arguments:
        name - The name of the tab to be deleted.
        """
        self.nb.forget(self.windowList[name])
        self.userMenu.setRoomList(self.windowList)
        self.roomMenu.setRoomList(self.windowList)

    def noDuplicate(self,name):
        """

        Checks if the window already exists

        Keyword arguments: 
        name - The name of the room.

        Return: 
        Zero if the name already exists else one. 
        """
        if name in self.windowList:
            return 0
        else:
            return 1

    def checkConnectQueue(self,thread):
        """

        Parses the result from a connection attempt. 

        Keyword arguments:
        thread - The thread attempting to connect.

        """
        result = thread.returnQueue()
        if (result == "empty"):
            self.master.after(500,self.checkConnectQueue,thread)
        elif (result == "Connected"):
            self.message.config(state=NORMAL)
            self.socketStatus = "ok"
            self.writeMessage("Du ar nu ansluten till " + self.configList["ipAdress"] + "!","syscall")
            self.Start()
            self.sendUserName()
            self.timeout_update()
            self.message.delete(0,END)
            if self.configList["restoreTabs"] == "auto" and len(self.windowList) > 1:
                self.restoreTabs()
            else:
                self.deleteAllTabs()
                self.clearWindowList()
                
        elif (result == "Failed"):
            self.writeMessage("Ateranslutning misslyckades, anslut manuellt med /connect IP","syscall")
            self.message.config(state=NORMAL)
            self.message.delete(0,END)
        else:
            self.writeMessage("Inget svar fran servern... Forsoker igen om " + self.configList["delay"] + " sekunder. " + str(result) + " forsok kvar","syscall")
            self.master.after(2000,self.checkConnectQueue,thread)

    
    def reconnect(self):
        """

        Starts the connect thread and initiate the checkConnectQueue function.
        """
        self.message.config(state=DISABLED)
        thread = connectToServer(self.serverSocket,self.configList["ipAdress"],self.configList["delay"],self.configList["reconnectAmount"])
        thread.daemon = True
        thread.start()
        self.checkConnectQueue(thread)

    def writeMessage(self,message,flag):
        """

        Displays the message in the currently active window.
        If the flag is set to syscall the text color is set to red. 
        """
        self.windowList[self.currentTab].config(state=NORMAL)
        self.windowList[self.currentTab].insert(END,message+'\n')
        if flag == "syscall":
           self.windowList[self.currentTab].tag_add("here", "end -2 line linestart", "end -2 line lineend")
           self.windowList[self.currentTab].tag_config("here", background="#121A16", foreground="red")
        self.windowList[self.currentTab].yview(END)
        self.windowList[self.currentTab].config(state=DISABLED)

    def initiateConfig(self):
        """

        Reads all the information from the configurationfile. 
        """
        file = open('configFile','r')
        for line in file:
            self.parseConfig(line)

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

    def restoreTabs(self):
        """

        Restores all the rooms the user was in.
        """
        for element in self.windowList:
            if element != "global":
                temp = element + " " + "/invite "+self.configList["userName"] + '\n'
                msg = temp.encode('UTF-8')
                self.serverSocket.send(msg)         

    def clearWindow(self):
        """

        Removes all text from the currently active window.
        """
        self.windowList[self.currentTab].config(state=NORMAL)
        self.windowList[self.currentTab].delete("1.0",END)
        self.windowList[self.currentTab].config(state=DISABLED)

    def deleteAllTabs(self):
        """

        deletes all tabs except for global.
        """
        for window in self.windowList:
            if window != "global":
                self.deleteTab(window)

    def rename(self):
        """

        Updates the configurationfile.
        """
        open('configFile', 'w').close()
        file = open('configFile','w')
        for element in self.configList:
            file.write(element+"="+self.configList[element]+'\n')
        file.close()

    def disconnect(self):
        """

        Terminates the connectin to the server.
        """
        if self.socketStatus == "ok":            
            self.serverSocket.shutdown(socket.SHUT_RDWR)
        self.serverSocket.close()
        self.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socketStatus = "disconnected"

    def clearWindowList(self):
        """

        Clears the entire windowList except for global. 
        """
        copyList = []
        for element in self.windowList:
            if element != "global":
                copyList.append(copy.deepcopy(element))    
        for key in copyList:
            if key == "global":
                continue
            else:
                self.windowList.pop(key,None)
                
    def timeout_update(self):
        if self.socketStatus == "ok":
            text = "/global " + "timeout_update\n"
            msg = text.encode('UTF-8')
            self.serverSocket.send(msg)
            self.master.after(30000,self.timeout_update)
        else:
            pass

    

if __name__ == "__main__":
    """

    The main function.
    """
    root=Tk()
    root.geometry("810x408")
    root.configure(background="#121A16")
    root.title("Nuntii IRC")
    m=GUI(root)
    m.initiateConfig()
    m.welcome()
    if m.configList["connectMode"] == 'auto':
        m.reconnect()
    else:
        m.writeMessage("Du ar inte ansluten till en server, anslut med /connect IP","syscall") 
        
    root.mainloop()
       
    
