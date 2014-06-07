import tkinter as tki

class UserMenu():
    def __init__(self,master,socket,userName):
        """

    	Creates the menu to be used for a list of users

    	Key arguments:
    		master - The frame to display the menu in
    		socket - The socket to send messages to
    		userName - The username of the user
    	"""
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
		name - The name of the other user to whisper to
	Side effects:
		A new private room is created	
        """

        self.sendCommand("global /join",name+self.userName+" private")
        self.master.after(300,self.sendCommand,name+self.userName+" /invite",name)

    def whois(self,name):

        """

        Sends a request to the server for information where a user
        is connected from

        Key arguments:
        	name - The user to request information about
        """
        self.sendCommand("global /whois",name)
        
    def track(self,name):
        """

    	Sends a request to the server for all the rooms a user is a member of

    	Key arguments:
    		name - The user to request information about
    	"""
        self.sendCommand("global /track",name)
        
    def setCurrent(self,name):
        """

    	Changes to the currently selected user in the parent

    	Key arguments:
    		name - The name of the selected user
    	"""
        self.current = name

    def popup(self, event):
        """

        Displays the menu

    	Key arguments:
    		event - The user right clicks
    	"""
        
        self.menu.post(event.x_root, event.y_root)

    def sendCommand(self,command,message):
        """

    	Build, convert and sends a string to the server 

    	Key arguments:
    		command - The command the user wants to perform
    		message - The argument to use with the command
    	"""
        
        msg_temp = command + " " + message +'\n'
        msg = msg_temp.encode('UTF-8')
        self.socket.send(msg)

    def setRoomList(self,roomList):
        """

    	Updates the list of which rooms the user is currently a member of

    	Key arguments:
    		roomList - A new list of which the user is currently a member of
    	"""
        self.roomList = roomList

    def rename(self,newName):
        """

    	Changes the users username

    	Key arguments:
    		newName - The new username
    	"""
        self.userName = newName

class RoomMenu():
    def __init__(self,master,socket):
        """

    	Creates the menu to be used with a list of available rooms

    	Key arguments:
    		master - The in which to display the menu
    		socket - The socket to send messages to
    	"""
        
        self.current = ""
        self.roomList = {}
        self.socket = socket
        self.menu = tki.Menu(master,tearoff=0)
        self.menu.add_command(label = "Join", command = lambda: self.join(self.current))
        self.menu.add_command(label = "Close Menu", command = self.menu.unpost)

    def join(self, name):
        """

    	Sends a request to join the selected room

    	Key arguments:
    		name - The name of the room to join
    	"""
        
        if name in self.roomList:
            pass
        else:
            self.sendCommand("global /join",name)
        
    def popup(self, event):
        """

    	Displays the meny

    	Key arguments:
    		event - The user right clicks
    	"""
        
        self.menu.post(event.x_root, event.y_root)

    def sendCommand(self,command,message):
        """

    	Build, convert and sends a string to the server 

    	Key arguments:
    		command - The command the user wants to perform
    		message - The argument to use with the command
    	"""
        
        msg_temp = command + " " + message +'\n'
        msg = msg_temp.encode('UTF-8')
        self.socket.send(msg)

    def setCurrent(self,name):
        """

    	Changes to the currently selected user in the parent

    	Key arguments:
    		name - The name of the selected user
    	"""
        
        self.current = name

    def setRoomList(self,roomList):
        """

    	Updates the list of which rooms the user is currently a member of

    	Key arguments:
    		roomList - A new list of which the user is currently a member of
    	"""
        self.roomList = roomList
