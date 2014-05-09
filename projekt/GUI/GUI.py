import tkinter
import socket
from tkinter import *
import time
import sys
from threading import Thread
import threading
from RecvThread import StoppableThread

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
    

    def closeDown(self):
        pass


###################################################
#Initierar huvudframen för GUI:t och sätter titeln
###################################################
        
class GUI(object):

    def __init__(self,master):
        self.master = master
        self.userName = ""
        
#########################################################################
#Lägger till ett textfält för chathistorik, och en scrollbar
#########################################################################

        self.scrollBar = Scrollbar(master)
        self.scrollBar.pack(side = RIGHT, fill = Y)
        self.chatHistory = Text(master, yscrollcommand=self.scrollBar.set,state=DISABLED)
        self.chatHistory.pack(side = TOP, fill=BOTH)
        self.scrollBar.config(command=self.chatHistory.yview)

#########################################################################
#Overridar "kryssnerstängningen" med en egen fuktion som closar sockets
#och signalerar undertråden att stänga ner sig själv
#########################################################################

        self.master.protocol('WM_DELETE_WINDOW', self.closeConnection)

#########################################################################
#Stringvariabel so används för att hämta texten från inmatningsfältet
#########################################################################

        self.temp = StringVar()

#########################################################################
#Öppnar socketen till servern på adressen, på port 1337
#########################################################################

        sockSend = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        #sockSend.connect(('46.246.18.140', 1337))
        #sockSend.connect(('130.243.209.116', 1337))
        sockSend.connect(('localhost', 1337))


###################################################
#Lägger till en send-knapp
###################################################

        sendButton = tkinter.Button(master,text = "Skicka", command = self.sendMessageButton)
        sendButton.pack(side = BOTTOM)

##################################################################
#Lägger till ett inmatningsfält, och binder Enterknappen till det
##################################################################

        self.message = Entry(master,textvariable = self.temp,width=40)
        self.message.pack(side = BOTTOM)
        self.message.bind('<Return>',self.sendMessage)

###################################################
#Importerar nuvarande tiden och returnerar den
###################################################

    def GetTime(self):
        return "<" + time.strftime("%H:%M")+">: "

##################################################################
#Hämtar texten från entryfältet och skickar den i bytearray-format
#till socketen. Tömmer sedan entryfältet
##################################################################

    def sendMessage(self,event):
     
        mtext1 = self.temp.get()
        if (mtext1 != ""):
            mtext = mtext1+'\n'
            msg = mtext.encode('UTF-8')
            self.sockSend.send(msg)
            self.message.delete(0,END)

##################################################################
#Samma som ovan, men utan event-argumentet då detta ej behövs
#vid command = sendMessage på knapppen
##################################################################

    def sendMessageButton(self):
     
        mtext1 = self.temp.get()
        if (mtext1 != ""):
            mtext = mtext1+'\n'
            msg = mtext.encode('UTF-8')
            self.sockSend.send(msg)
            self.message.delete(0,END)

##################################################################
#Stänger ner connectionen när man trycker krysset
##################################################################

    def closeConnection(self):
        self.sockSend.shutdown(socket.SHUT_RDWR)
        self.sockSend.close()
        print("Nu drar mainthread, see ya suckerzzzzzz!")
        self.thread.stop()
        self.master.destroy()
        sys.exit(0)

##########################################################
#Startar bakgrundsfunktionen som kontinuerligt kollar om
#vi har fått nytt meddelande
##########################################################

    def Start(self):
        self.thread = StoppableThread(self.sockSend)
        self.thread.daemon = True
        self.thread.start()
        self.checkQueue()

##########################################################
#Kollar om det finns något nytt meddelande att hämta
##########################################################

    def checkQueue(self):
    
        respons = self.thread.returnQueue()
        if (respons == "empty"):
            self.master.after(50,self.checkQueue)
        else:
            self.chatHistory.config(state=NORMAL)
            self.chatHistory.insert(INSERT,self.GetTime() + respons)
            self.chatHistory.config(state=DISABLED)
            self.master.after(50,self.checkQueue)

    def enterUserName(self):
        self.popup = popupWindow(self.master)
        self.master.wait_window(self.popup.top)

    def getUserName(self):
        return self.popup.value

    def welcome(self):
        self.chatHistory.config(state=NORMAL)
        self.chatHistory.insert(END,"Välkommen tillbaka "+self.userName +"!\n")
        self.chatHistory.insert(END,"----------------------------------------\n")
        self.chatHistory.config(state=DISABLED)

    def sendUserName(self):
        userName = "USER "+self.userName
        msg = userName.encode('UTF-8')
        self.sockSend(msg)



###################################################
#Startar GUI:t
###################################################

if __name__ == "__main__":
    root=Tk()
    root.title("Nuntii IRC")
    m=GUI(root)
    root.withdraw()
    m.enterUserName()
    m.userName = m.getUserName()
    m.sendUserName
    m.welcome()    
    root.deiconify()
    print(m.userName)
    m.Start()
    


