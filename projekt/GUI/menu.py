import tkinter as tki

class UserMenu():
    def __init__(self,master,socket,userName):
        self.master = master
        self.current = ""
        self.userName = userName
        self.socket = socket
        self.menu = tki.Menu(master,tearoff=0)
        self.menu.add_command(label = "Whisper", command = lambda: self.whisper(self.current))
        self.menu.add_command(label = "Whois",command = lambda: self.whois(self.current))
        self.menu.add_command(label = "Track",command = lambda: self.track(self.current))
        self.menu.add_command(label = "Dummy",command = self.menu.unpost)
        self.menu.add_command(label = "Dummy",command = self.menu.unpost)


    def createRoomMenu(self,roomList):
        """
	
	Creates the list of rooms to be used with the invite option
	
	Key arguments:
		roomList - A list of the rooms the user is a member of
	Sideeffect:
		userMenu updated with the new submenu
	"""

        roomMenu = tki.Menu(self.menu,tearoff=0)
        
        def createRoomMenuAux(menu,text):
            menu.add_command(label=text,command = lambda: self.invite(text,self.current))

        for roomName in roomList:
            createRoomMenuAux(roomMenu,roomName)
        
        self.menu.delete(3,5)
        self.menu.add_cascade(label="Invite", menu=roomMenu)
        self.menu.add_command(label = "Close",command = self.menu.unpost)
	
    def invite(self,roomName,user):
        """

	Invites another user to a room the user is a member of

	Key arguments: 
		user - The name of the user to invite
		roomName - The room to invite to
        """

        self.sendCommand(roomName +" /invite",user)
	
    def whisper(self,name):
        """

	Starts a private conversation with another user

	Key arguments: 
		namn - The name of the other user to whisper to
	Side effects:
		A new private room is created	
        """

        self.sendCommand("global /join",name+self.userName+" private")
        self.master.after(300,self.sendCommand,name+self.userName+" /invite",name)

    def whois(self,namn):
	
        self.sendCommand("global /whois",namn)
    def track(self,namn):
        self.sendCommand("global /track",namn)
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

    def rename(self,newName):
        self.userName = newName

class RoomMenu():
    def __init__(self,master,socket):
        
        self.current = ""
        self.roomList = {}
        self.socket = socket
        self.menu = tki.Menu(master,tearoff=0)
        self.menu.add_command(label = "Join", command = lambda: self.join(self.current))
        self.menu.add_command(label = "Close Menu", command = self.menu.unpost)

    def join(self, namn):
        if namn in self.roomList:
            pass
        else:
            self.sendCommand("global /join",namn)
        
    def popup(self, event):
        self.menu.post(event.x_root, event.y_root)
        
    def popupFocusOut(self,event=None):
        self.menu.unpost()

    def sendCommand(self,command,message):
        msg_temp = command + " " + message +'\n'
        msg = msg_temp.encode('UTF-8')
        self.socket.send(msg)

    def setCurrent(self,name):
        self.current = name

    def setRoomList(self,roomList):
        self.roomList = roomList

