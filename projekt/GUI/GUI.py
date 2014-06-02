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


##########################################################
#Initierar GUI:t
##########################################################    

class GUI(object):

    def __init__(self,master):

        self.master = master
        Gstyle = ttk.Style()
        Gstyle.configure("TNotebook", background="#121A16", borderwidth=0)
        Gstyle.configure("TNotebook.Tab", background='#545854',foreground="black",borderwidth=1)
        
##########################################################
#Initierar Notebook widgeten
########################################################## 

        self.nb = ttk.Notebook(master,style='TNotebook')
        self.nb.place(x=122, y=0)

##########################################################
#Userlist där alla användarna i ett rum ska listas
########################################################## 

        self.userWindow = Listbox(master, width=15,height=24)
        self.userWindow.config(background="#121A16",foreground="#00EB00",highlightthickness=0)
        self.userWindow.place(x=0,y=23)
        self.roomWindow = Listbox(master, width=15,height=24)
        self.roomWindow.config(background="#121A16",foreground="#00EB00",highlightthickness=0)
        self.roomWindow.place(x=689,y=23)
        self.roomLabel = Label(master,text="Tillgängliga Rum",font=("Helvetica",10))
        self.roomLabel.config(background="#121A16",foreground="#00EB00")
        self.roomLabel.place(x=695,y=0)
        self.userLabel = Label(master,text="Användare",font=("Helvetica",10))
        self.userLabel.config(background="#121A16",foreground="#00EB00") 
        self.userLabel.place(x=30,y=0)
       

##########################################################################
#Stringvariablel som används för att få tillbaka texten från Entryfältet
##########################################################################

        self.temp = StringVar()

#########################################################################
#Initierar Entryfältet där användaren skriver in sina meddelanden
#########################################################################

        self.message = Entry(master,width=70,textvariable = self.temp)
        self.message.config(background = "#121A16",foreground="#00EB00",insertbackground="#00EB00") 
        self.message.place(x=123,y=388)
        self.message.bind('<Return>',self.sendMessage)
        
############################################################################
#Initierar ett dictionary för att hålla koll på alla fönsternamn som skapas
############################################################################

        self.windowList = {}
        self.userList = {}
        self.rooms = {}
        self.configList = {}

        self.userList["global"] = ['']
                
#################################
#Användarnamnet
#################################
        
        self.configList["userName"] = ""
        self.Erik = 0

###########################################
#Anger om socketen är ansluten eller inte
###########################################

        self.socketStatus = "disconnected"
        self.config = ()

#######################################################################################
#Anger den nu aktiva taben. Ändras automatiskt när man byter tab     
#######################################################################################

        self.currentTab = "global"
        self.nb.bind_all("<<NotebookTabChanged>>", self.tabChangedEvent)

#####################################################################
#Initierar socketen
#####################################################################
        
        self.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

#####################################################################
#Overridar fönstrets orginal-stängningsfunktion   
##################################################################### 
        
        self.master.protocol('WM_DELETE_WINDOW', self.closeConnection)

#####################################################################
#Skapar och registrerar global-rummet
#####################################################################

        self.globalRoom = Text(master,state=DISABLED)
        self.globalRoom.config(background = "#121A16",foreground="#00EB00")
        self.nb.add(self.globalRoom, text='global')
        self.windowList["global"] = self.globalRoom

#######################################################################################
#Skapar ett nytt fönster och tab och lägger till dem i dictionaryt över fönster      
#######################################################################################
        
    def addTab(self,name):
        tab = Text(self.master,state=DISABLED)
        tab.config(background = "#121A16",foreground="#00EB00")
        self.windowList[name] = tab
        self.userMenu.createRoomMenu(self.windowList)
        self.nb.add(tab, text=name)
        self.userMenu.setRoomList(self.windowList)
        self.roomMenu.setRoomList(self.windowList)

###################################################
#Importerar nuvarande tiden och returnerar den
###################################################
        
    def GetTime(self):
        return "<" + time.strftime("%H:%M")+" "

##################################################################
#Hämtar texten från entryfältet och skickar den i bytearray-format
#till socketen. Tömmer sedan entryfältet. Parsar även de möjliga
#kommandona man kan utföra
##################################################################

    def sendMessage(self,event):

        mtext1 = self.temp.get()
        argumentString = self.messageSplit(mtext1)

        if (argumentString[0] == "/connect"):
            if(self.socketStatus == "disconnected"):
               self.configList["ipAdress"] = argumentString[1]
               self.reconnect()
            else:
                if argumentString[1] == self.configList["ipAdress"]:
                    self.writeMessage("Du är redan ansluten till " +argumentString[1],"syscall")
                else:
                    self.disconnect()
                    self.configList["ipAdress"] = argumentString[1]
                    self.reconnect()
                self.message.delete(0,END)
            
                    
        elif (self.socketStatus != "ok"):
            self.writeMessage("Du är inte ansluten till en server, anslut med /connect IP","syscall")
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
                self.writeMessage("Du är redan med i det angivna rummet!","syscall")
                self.message.delete(0,END)

        elif (argumentString[0] == "/invite"):
            msg_temp =self.currentTab + " " + mtext1+'\n'
            msg = msg_temp.encode('UTF-8')
            self.serverSocket.send(msg)
            self.message.delete(0,END)
        elif (argumentString[0] == "/clea"):
            self.clearWindow()

        elif (argumentString[0] == "/rename"):
            
            msg_temp =self.currentTab + " " + mtext1+'\n'
            msg = msg_temp.encode('UTF-8')
            self.serverSocket.send(msg)
            self.message.delete(0,END)
            
        elif (argumentString[0] == "/exit"):
            if argumentString[1] == "global":
                self.writeMessage("Du kan inte gå ur global!","syscall")
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
                    self.writeMessage("Du är inte inne i rummet: " + argumentString[1]+"!","syscall")
                    self.message.delete(0,END)
        else:
            mtext = self.currentTab + " " + mtext1+'\n'
            msg = mtext.encode('UTF-8')
            self.serverSocket.send(msg)
            self.message.delete(0,END)

##################################################################
#Stänger ner connectionen när man trycker krysset
##################################################################

    def closeConnection(self):
        if self.socketStatus == "ok":            
            self.serverSocket.shutdown(socket.SHUT_RDWR)
        self.serverSocket.close()
        self.master.destroy()
        sys.exit(0)
        
##########################################################
#Startar bakgrundsfunktionen som kontinuerligt kollar om
#vi har fått nytt meddelande
##########################################################

    def Start(self):
        self.thread = StoppableThread(self.serverSocket)
        self.thread.daemon = True
        self.thread.start()
        self.checkQueue()
        self.initiateMenues()
        self.userMenu.setCurrent(self.configList["userName"])
        self.roomMenu.setCurrent("global")
        

#########################################################

    def initiateMenues(self):
        self.userMenu = UserMenu(self.master,self.serverSocket,self.configList["userName"])
        self.userWindow.bind('<<ListboxSelect>>',self.userSelect)
        self.userWindow.bind('<Button-3>',self.userMenu.popup)
      
        self.userMenu.createRoomMenu(self.windowList)

        self.roomMenu = RoomMenu(self.master,self.serverSocket)
        self.roomWindow.bind('<<ListboxSelect>>',self.roomSelect)
        self.roomWindow.bind('<Button-3>',self.roomMenu.popup)
        
    def userSelect(self,event):
        
        self.userMenu.setCurrent(self.userWindow.get(self.userWindow.curselection()))

    def roomSelect(self,event):
 
        self.roomMenu.setCurrent(self.roomWindow.get(self.roomWindow.curselection()))
 
    def userSelectAlt(self):
        self.userMenu.setCurrent(self.userWindow.get(self.userWindow.curselection()))
    def roomSelectAlt(self):
        self.roomMenu.setCurrent(self.roomWindow.get(self.roomWindow.curselection()))
       

##########################################################
#Kollar om det finns något nytt meddelande att hämta
#Tolkar även specialmeddelanden från servern
##########################################################

    def checkQueue(self):
        
        stopSign = 1
        respons = self.thread.returnQueue()
        if (respons == "empty"):
            1+1
        elif(respons == "Disconnected"):
            self.socketStatus = "disconnected"
            self.disconnect()
            stopSign = 0
            if self.configList["reconnectMode"] == 'auto':
                self.writeMessage("Tappade anslutningen, försöker återansluta automatiskt","syscall")
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
                        self.writeMessage("Rummet är slutet, du måste bli inbjuden","syscall")
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
                        self.writeMessage("------------------------","syscall")
                        self.writeMessage("Användaren är med i följande rum:","syscall")
                        for element in trackList:
                            self.writeMessage(element,"syscall")
                        self.writeMessage("------------------------","syscall")
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

##########################################################
#Fyller userList med användarna i det aktuella rummet
##########################################################

    def fillUserList(self,roomName):
        self.userWindow.delete(0,END)
        for userName in self.userList[roomName]:     
            self.userWindow.insert(END,userName)

##########################################################
#Skriver ut välkomstmeddelandet
##########################################################

    def welcome(self):
        self.globalRoom.config(state=NORMAL)
        self.globalRoom.insert(END,"Välkommen "+self.configList["userName"] +"!\n")
        self.globalRoom.insert(END,"----------------------------------------\n")
        self.globalRoom.config(state=DISABLED)

##########################################################
#Returnerar det angivna användarnamnet
##########################################################

    def getUserName(self):
        return self.popup.value

##########################################################
#Skickar vårt användarnamn till servern
##########################################################

    def sendUserName(self):
        userName = self.configList["userName"]
        temp = userName+'\n'
        msg = temp.encode('UTF-8')
        self.serverSocket.send(msg)

########################################################################################
#Uppdaterar self.currentTab till den nya aktuella taben varje gång användaren byter tab
########################################################################################

    def tabChangedEvent(self,event):
        self.currentTab = event.widget.tab(event.widget.index("current"),"text")
        self.fillUserList(self.currentTab)

##########################################################
#Splittar upp en mottagen sträng från servern
##########################################################
        
    def messageSplit(self,input):
        index = input.find(" ")
        message = (input[0:index],input[index+1:len(input)])
        return message

############################################################
#Tar bort den angivna taben och det relaterade chatfönstret
############################################################

    def deleteTab(self,name):
        self.nb.forget(self.windowList[name])
        self.userMenu.setRoomList(self.windowList)
        self.roomMenu.setRoomList(self.windowList)
###################################################################
#Kolla om det angivna namnet redan existerar i listan över fönster
###################################################################

    def noDuplicate(self,name):
        if name in self.windowList:
            return 0
        else:
            return 1

##########################################################
#Upprepar periodiska anslutningsförsök 5 gånger och utför
#lite stuff beroende på om den lyckas eller inte
##########################################################

    def checkConnectQueue(self,thread):
        result = thread.returnQueue()
        if (result == "empty"):
            self.master.after(500,self.checkConnectQueue,thread)
        elif (result == "Connected"):
            self.message.config(state=NORMAL)
            self.socketStatus = "ok"
            self.writeMessage("Du är nu ansluten till " + self.configList["ipAdress"] + "!","syscall")
            self.Start()
            self.sendUserName()
            self.message.delete(0,END)
            if self.configList["restoreTabs"] == "auto" and len(self.windowList) > 1:
                self.restoreTabs()
            else:
                self.deleteAllTabs()
                self.clearWindowList()
                
        elif (result == "Failed"):
            self.writeMessage("Återanslutning misslyckades, anslut manuellt med /connect IP","syscall")
            self.message.config(state=NORMAL)
            self.message.delete(0,END)
        else:
            self.writeMessage("Inget svar från servern... Försöker igen om " + self.configList["delay"] + " sekunder. " + str(result) + " försök kvar","syscall")
            self.master.after(2000,self.checkConnectQueue,thread)
    
    def reconnect(self):
        self.message.config(state=DISABLED)
        thread = connectToServer(self.serverSocket,self.configList["ipAdress"],self.configList["delay"],self.configList["reconnectAmount"])
        thread.daemon = True
        thread.start()
        self.checkConnectQueue(thread)
        

##########################################################
#Skriver ut ett meddelande i det aktiva fönstret
##########################################################

    def writeMessage(self,message,flag):
        self.windowList[self.currentTab].config(state=NORMAL)
        self.windowList[self.currentTab].insert(END,message+'\n')
        if flag == "syscall":
           self.windowList[self.currentTab].tag_add("here", "end -2 line linestart", "end -2 line lineend")
           self.windowList[self.currentTab].tag_config("here", background="#121A16", foreground="red")
        self.windowList[self.currentTab].yview(END)
        self.windowList[self.currentTab].config(state=DISABLED)

###########################################################################################
#Initierar den angivna konfigurationen från configFilen mha hjälpfunktionen parseConfig
###########################################################################################

    def initiateConfig(self):
        file = open('configFile','r')
        for line in file:
            self.parseConfig(line)

    def parseConfig(self,configString):
        index = configString.find("=")
        element = configString[0:index]
        message = configString[index+1:len(configString)-1]
        self.configList[element] = message


##########################################################
#Skriver ut ett meddelande i det aktiva fönstret
##########################################################

    def restoreTabs(self):
        for element in self.windowList:
            if element != "global":
                temp = element + " " + "/invite "+self.configList["userName"] + '\n'
                msg = temp.encode('UTF-8')
                self.serverSocket.send(msg)         

    def clearWindow(self):
        self.windowList[self.currentTab].config(state=NORMAL)
        self.windowList[self.currentTab].delete("1.0",END)
        self.windowList[self.currentTab].config(state=DISABLED)

    def deleteAllTabs(self):
        for window in self.windowList:
            if window != "global":
                self.deleteTab(window)

    def rename(self):
        open('configFile', 'w').close()
        file = open('configFile','w')
        for element in self.configList:
            file.write(element+"="+self.configList[element]+'\n')
        file.close()

    def disconnect(self):
        if self.socketStatus == "ok":            
            self.serverSocket.shutdown(socket.SHUT_RDWR)
        self.serverSocket.close()
        self.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socketStatus = "disconnected"

    def clearWindowList(self):
        copyList = []
        for element in self.windowList:
            if element != "global":
                copyList.append(copy.deepcopy(element))    
        for key in copyList:
            if key == "global":
                continue
            else:
                self.windowList.pop(key,None)       

##########################################################
#Startar mainfunktionen
##########################################################

if __name__ == "__main__":
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
        m.writeMessage("Du är inte ansluten till en server, anslut med /connect IP","syscall") 
        
    root.mainloop()
       
    
