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
        #self.nb.pack(side=RIGHT

##########################################################
#Userlist där alla användarna i ett rum ska listas
########################################################## 

        self.userList = Text(master, width=20,state=DISABLED)
        self.userList.place(x=0,y=23)

##########################################################################
#Stringvariablel som används för att få tillbaka texten från Entryfältet
##########################################################################

        self.temp = StringVar()

#########################################################################
#Initierar Entryfältet där användaren skriver in sina meddelanden
#########################################################################

        self.message = Entry(master,width=40,textvariable = self.temp)
        self.message.place(x=260,y=400)
        self.message.bind('<Return>',self.sendMessage)

############################################################################
#Initierar ett dictionary för att hålla koll på alla fönsternamn som skapas
############################################################################

        self.windowList = {}
        
#################################
#Användarnamnet
#################################
        
        self.userName = ""

#######################################################################################
#Sparar namnt för det aktiva rummet och appendar det varje gång ett meddelande skickas
#Detta för att slippa skriva ut ex "global Hejsan" varje gång        
#######################################################################################

        self.currentTab = "global"
        self.nb.bind_all("<<NotebookTabChanged>>", self.tabChangedEvent)


#####################################################################
#Initierar och ansluter socketen till servern     
#####################################################################        

        self.sockSend = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        #self.sockSend.connect(('46.246.18.140', 1337))
        self.sockSend.connect(('localhost', 1337))
        #self.sockSend.connect(('130.243.207.26', 1337))

        self.master.protocol('WM_DELETE_WINDOW', self.closeConnection)

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
<<<<<<< HEAD
        return "<" + time.strftime("%H:%M")+">: "
=======
        return "<" + time.strftime("%H:%M")+" "
>>>>>>> persnusk

##################################################################
#Hämtar texten från entryfältet och skickar den i bytearray-format
#till socketen. Tömmer sedan entryfältet
##################################################################

    def sendMessage(self,event):
     
        mtext1 = self.temp.get()
        if (mtext1 != ""):
            argumentString = self.messageSplitLocal(mtext1)
         
            
            if (argumentString[0] == "/join"):
<<<<<<< HEAD
                self.addTab(argumentString[1])
                msg_temp = self.currentTab + " " + mtext1+'\n'
                msg = msg_temp.encode('UTF-8')
                self.sockSend.send(msg)
                self.message.delete(0,END)
            elif (argumentString[0] == "/exit"):
                self.nb.index(argumentString[1])
=======
                if self.noDuplicate(argumentString[1]):
                    self.addTab(argumentString[1])
                    msg_temp = self.currentTab + " " + mtext1+'\n'
                    msg = msg_temp.encode('UTF-8')
                    self.sockSend.send(msg)
                    self.message.delete(0,END)
                else:
                    self.windowList[self.currentTab].config(state=NORMAL)
                    self.windowList[self.currentTab].insert(INSERT,"Du är redan med i rummet!\n")
                    self.windowList[self.currentTab].config(state=DISABLED)
                    self.message.delete(0,END)
                    
            elif (argumentString[0] == "/exit"):
                self.deleteTab(argumentString[1])
                self.windowList.pop(argumentString[1],None)
>>>>>>> persnusk
                msg_temp = self.currentTab + " " + mtext1+'\n'
                msg = msg_temp.encode('UTF-8')
                self.sockSend.send(msg)
                self.message.delete(0,END)
            else:
                mtext = self.currentTab + " " + mtext1+'\n'
                msg = mtext.encode('UTF-8')
                self.sockSend.send(msg)
                self.message.delete(0,END)
<<<<<<< HEAD

##################################################################
#Stänger ner connectionen när man trycker krysset
##################################################################

=======

##################################################################
#Stänger ner connectionen när man trycker krysset
##################################################################

>>>>>>> persnusk
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
<<<<<<< HEAD

    def checkQueue(self):
    
        respons = self.thread.returnQueue()
        if (respons == "empty"):
            self.master.after(50,self.checkQueue)
        else:
            argumentString = self.messageSplitLocal(respons)
            print(argumentString)
            self.windowList[argumentString[0]].config(state=NORMAL)
            self.windowList[argumentString[0]].insert(INSERT,self.GetTime() + argumentString[1])
            self.windowList[argumentString[0]].config(state=DISABLED)
            self.master.after(50,self.checkQueue)

##########################################################
#Startar upp popupfönster för att ange användarnamn
##########################################################

    def enterUserName(self):
        self.popup = popupWindow(self.master)
        self.master.wait_window(self.popup.top)

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
        self.sockSend.send(msg)

########################################################################################
#Uppdaterar self.currentTab till den nya aktuella taben varje gång användaren byter tab
########################################################################################

    def tabChangedEvent(self,event):
        self.currentTab = event.widget.tab(event.widget.index("current"),"text")
        
    def messageSplitLocal(self,input):
        index = input.find(" ")
        
        message = (input[0:index],input[index+1:len(input)])
        return message

if __name__ == "__main__":
    root=Tk()
    root.geometry("700x500")
    root.title("Nuntii IRC")
    m=GUI(root)
    root.withdraw()
    m.enterUserName()
    m.userName = m.getUserName()
    m.sendUserName()
    m.welcome()    
    root.deiconify()
    m.Start()
    root.mainloop()	
=======

    def checkQueue(self):
    
        respons = self.thread.returnQueue()
        if (respons == "empty"):
            self.master.after(50,self.checkQueue)
        else:
            argumentString = self.messageSplitLocal(respons)
            print(argumentString)
            self.windowList[argumentString[0]].config(state=NORMAL)
            self.windowList[argumentString[0]].insert(INSERT,self.GetTime() + argumentString[1])
            self.windowList[argumentString[0]].config(state=DISABLED)
            self.master.after(50,self.checkQueue)

##########################################################
#Startar upp popupfönster för att ange användarnamn
##########################################################

    def enterUserName(self):
        self.popup = popupWindow(self.master)
        self.master.wait_window(self.popup.top)

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
        self.sockSend.send(msg)

########################################################################################
#Uppdaterar self.currentTab till den nya aktuella taben varje gång användaren byter tab
########################################################################################

    def tabChangedEvent(self,event):
        self.currentTab = event.widget.tab(event.widget.index("current"),"text")
        
    def messageSplitLocal(self,input):
        index = input.find(" ")
        
        message = (input[0:index],input[index+1:len(input)])
        return message

    def deleteTab(self,name):
        self.nb.forget(self.windowList[name])

    def noDuplicate(self,name):
        if name in self.windowList:
            return 0
        else:
            return 1

if __name__ == "__main__":
    root=Tk()
    root.geometry("700x500")
    root.title("Nuntii IRC")
    m=GUI(root)
    root.withdraw()
    m.enterUserName()
    m.userName = m.getUserName()
    m.sendUserName()
    m.welcome()    
    root.deiconify()
    m.Start()
    root.mainloop()

>>>>>>> persnusk


