import tkinter as tki

class UserMenu():
    def __init__(self,master,socket):
        self.current = ""
        self.socket = socket
        self.menu = tki.Menu(master,tearoff=0)
        self.menu.add_command(label = "Whisper", command = lambda: self.whisper(self.current))
        self.menu.add_command(label = "Whois",command = lambda: self.whois(self.current))
        self.menu.add_command(label = "Track",command = lambda: self.track(self.current))
        self.menu.add_command(label = "Poke", command = lambda: self.poke(self.current))
        self.menu.add_command(label = "Close", command = lambda: self.menu.unpost)

    def whisper(self,namn):
            print ("Nu ska här viskas till " + namn +" !")

    def whois(self,namn):
        self.sendCommand("global /whois",namn)
    def track(self,namn):
        self.sendCommand("global /track",namn)

    def poke(self,namn):
        self.sendCommand("global /poke",namn)

    def setCurrent(self,name):
        self.current = name

    def popup(self, event):
        self.menu.post(event.x_root, event.y_root)

    def popupFocusOut(self,event=None):
        self.menu.unpost()

    def sendCommand(self,command,message):
        msg_temp = command + " " + message +'\n'
        msg = msg_temp.encode('UTF-8')
        self.socket.send(msg)

    def setRoomList(self,roomList):
        self.roomList = roomList

class RoomMenu():
    def __init__(self,master,socket):
        
        self.current = ""
        self.roomList = {}
        self.socket = socket
        self.menu = tki.Menu(master,tearoff=0)
        self.menu.add_command(label = "Join", command = lambda: self.join(self.current))
        self.menu.add_command(label = "List Users", command = lambda: self.listUsers(self.current))
        self.menu.add_command(label = "Close", command = self.menu.unpost)

    def join(self, namn):
        if namn in self.roomList:
            print("ROFLBBQ")
        else:
            self.sendCommand("global /join",namn)
        

    def popup(self, event):
        self.menu.post(event.x_root, event.y_root)
        
    def popupFocusOut(self,event=None):
        self.menu.unpost()

    def sendCommand(self,command,message):
        msg_temp = command + " " + message +'\n'
        msg = msg_temp.encode('UTF-8')
        print(msg)
        self.socket.send(msg)

    def setCurrent(self,name):
        self.current = name

    def setRoomList(self,roomList):
        self.roomList = roomList

