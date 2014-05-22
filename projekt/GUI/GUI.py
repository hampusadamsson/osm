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

##########################################################
#Klass för att skriva in användarnamnet
##########################################################

class popupWindow(object):
    def __init__(self,master):
        top=self.top=Toplevel(master)
        self.l=Label(top,text="Ange användarnamn")
        self.l.pack()
        self.e=Entry(top)
        self.e.pack()
        self.b=Button(top,text='Ok',command=self.cleanup)
        self.b.pack()
    def cleanup(self):
        self.value=self.e.get()
        if (self.value != ""):
            self.top.destroy()
        else:
            pass

##########################################################
#Initierar GUI:t
##########################################################    

class GUI(object):

    def __init__(self,master):

        self.master = master
        
##########################################################
#Initierar Notebook widgeten
########################################################## 

        self.nb = ttk.Notebook(master)
        self.nb.place(x=130, y=0)

##########################################################
#Userlist där alla användarna i ett rum ska listas
########################################################## 

        self.userWindow = Text(master, width=20,state=DISABLED)
        self.userWindow.place(x=0,y=23)

##########################################################################
#Stringvariablel som används för att få tillbaka texten från Entryfältet
##########################################################################

        self.temp = StringVar()

#########################################################################
#Initierar Entryfältet där användaren skriver in sina meddelanden
#########################################################################

        self.message = Entry(master,width=40,textvariable = self.temp)
        self.message.place(x=260,y=440)
        self.message.bind('<Return>',self.sendMessage)

############################################################################
#Initierar ett dictionary för att hålla koll på alla fönsternamn som skapas
############################################################################

        self.windowList = {}
        self.userList = {}
        self.roomSuccess = {}

        self.userList["global"] = ['Erik']
                
#################################
#Användarnamnet
#################################
        
        self.userName = ""

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
#Den senast angivna IP adressen   
#####################################################################

        self.ipAdress = 'localhost'
        #self.ipAdress = '46.246.19.138'
        #self.ipAdress = '130.243.207.26'

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
        self.nb.add(self.globalRoom, text='global')
        self.windowList["global"] = self.globalRoom

#######################################################################################
#Skapar ett nytt fönster och tab och lägger till dem i dictionaryt över fönster      
#######################################################################################
        
    def addTab(self,name):
        tab = Text(self.master,state=DISABLED)
        self.windowList[name] = tab
        self.nb.add(tab, text=name)    

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
               self.ipAdress = argumentString[1]
               self.reconnect()
            else:
               self.message.delete(0,END)
            
                    
        elif (self.socketStatus != "ok"):
            self.writeMessage("Du är inte ansluten till en server, anslut med /connect IP")
            self.message.delete(0,END)
            
        elif (argumentString[0] == "/join"):
            if self.noDuplicate(argumentString[1]):
                if (" " in argumentString[1]):
                    argumentString2 = self.messageSplit(argumentString[1])
                    self.addTab(argumentString2[0])
                    msg_temp = argumentString2[0] + " " + mtext1+'\n'
                    
                    msg = msg_temp.encode('UTF-8')
                    self.serverSocket.send(msg)
                    self.message.delete(0,END)
                else:
                    
                    msg_temp = argumentString[1] + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')
                    self.serverSocket.send(msg)
                    while (self.roomSuccess[argumentString[1]] != "success" and self.roomSuccess[argumentString[1]] != "error"):
                        continue
                    if (self.roomSuccess[argumentString[1]] == "success"):
                        self.addTab(argumentString[1])
                    else:
                        self.writeMessage("Rummet är slutet, du måste bli invitad!")
                    self.message.delete(0,END)
                    self.roomSuccess[argumentString[1]],none)
            else:
                self.writeMessage("Du är redan med i det angivna rummet!")
                self.message.delete(0,END)

        elif (argumentString[0] == "/invite"):
                argumentString2 = self.messageSplit(argumentString[1])
                if(not self.noDuplicate(argumentString2[1])):
                    msg_temp ="global" + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')
                 
                    self.serverSocket.send(msg)
                    self.message.delete(0,END)
                else:
                    self.writeMessage("Du kan inte bjuda in till ett rum du inte är medlem i!")
                    self.message.delete(0,END)

        elif (argumentString[0] == "/exit"):
            if argumentString[1] == "global":
                self.writeMessage("Du kan inte gå ur global!")
                self.message.delete(0,END)
            else:
                if(not self.noDuplicate(argumentString[1])):
                    self.deleteTab(argumentString[1])
                    self.windowList.pop(argumentString[1],None)
                    msg_temp ="global" + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')
                    self.serverSocket.send(msg)
                    self.message.delete(0,END)
                else:
                    self.writeMessage("Du är inte inne i rummet: " + argumentString[1]+"!")
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
        print("Nu drar mainthread, see ya suckerzzzzzz!\n")
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
            self.serverSocket.close()
            self.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            stopSign = 0
            if self.config[0] == 'auto':
                self.writeMessage("Tappade anslutningen, försöker återansluta automatiskt")
                self.reconnect()
            else:
                self.writeMessage("Tappade anslutningen till servern, anslut manuellt med /connect IP")           
        elif(respons[0][0] == "{"):
                temp = respons[1:len(respons)-2]
                commandString = self.messageSplit(temp)
                if (commandString[0] == "success" or commmandString[0] == "error"):
                    self.roomSuccess[commandString[1]] == commandString[0]
                else:
                    self.userList[commandString[0]] = commandString[1].split(",")
        else:
            argumentString = self.messageSplit(respons)          
            self.windowList[argumentString[0]].config(state=NORMAL)
            self.windowList[argumentString[0]].insert(INSERT,self.GetTime() + argumentString[1])
            self.windowList[argumentString[0]].config(state=DISABLED)
        if stopSign == 1:
            self.master.after(50,self.checkQueue)

##########################################################
#Fyller userList med användarna i det aktuella rummet
##########################################################

    def fillUserList(self,roomName):
        self.userWindow.config(state = NORMAL)
        self.userWindow.delete(1.0,END)
        for userName in self.userList[roomName]:     
            self.userWindow.insert(END,userName+'\n')
        self.userWindow.config(state = DISABLED)

##########################################################
#Startar upp popupfönster för att ange användarnamn
##########################################################

    def enterUserName(self):
        self.popup = popupWindow(self.master)
        self.master.wait_window(self.popup.top)

##########################################################
#Skriver ut välkomstmeddelandet
##########################################################

    def welcome(self):
        self.globalRoom.config(state=NORMAL)
        self.globalRoom.insert(END,"Välkommen tillbaka "+self.userName +"!\n")
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
        userName = self.userName
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
            self.writeMessage("Du är nu ansluten till " + self.ipAdress + "!")
            self.Start()
            self.sendUserName()
            self.message.delete(0,END)
        elif (result == "Failed"):
            self.writeMessage("Återanslutning misslyckades, anslut manuellt med /connect IP")
            self.message.config(state=NORMAL)
            self.message.delete(0,END)
        else:
            self.writeMessage("Inget svar från servern... Försöker igen om 5 sekunder. " + str(result) + " försök kvar")
            self.master.after(2000,self.checkConnectQueue,thread)
    
    def reconnect(self):
        self.message.config(state=DISABLED)
        thread = connectToServer(self.serverSocket,self.ipAdress,self.config[1])
        thread.daemon = True
        thread.start()
        self.checkConnectQueue(thread)
        

##########################################################
#Skriver ut ett meddelande i det aktiva fönstret
##########################################################

    def writeMessage(self,message):
        self.windowList[self.currentTab].config(state=NORMAL)
        self.windowList[self.currentTab].insert(INSERT,message+'\n')
        self.windowList[self.currentTab].config(state=DISABLED)

###########################################################################################
#Initierar den angivna konfigurationen från configFilen mha hjälpfunktionen parseConfig
###########################################################################################

    def initiateConfig(self):
        self.config = [self.parseConfig(line) for line in open('configFile')]

    def parseConfig(self,configString):
        index = configString.find("=")
        message = configString[index+1:len(configString)-1]
        return message
        

##########################################################
#Startar mainfunktionen
##########################################################

if __name__ == "__main__":
    root=Tk()
    root.geometry("700x500")
    root.title("Nuntii IRC")
    m=GUI(root)
    root.withdraw()
    m.initiateConfig()
    m.welcome()    
    m.enterUserName()
    m.userName = m.getUserName()
    root.deiconify()
    m.reconnect()
    root.mainloop()
       
    
