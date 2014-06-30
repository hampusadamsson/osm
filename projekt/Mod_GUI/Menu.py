import tkinter as tki
from tkinter import *

class Menues(object):

    def __init__(self,gui):
        self.master = gui.master
        self.socket = gui.serverSocket
        self.current = gui.configList["userName"]
        self.roomList = gui.windowList
        self.userWindow = gui.userWindow
        self.roomWindow = gui.roomWindow
        self.userName = gui.configList["userName"]
        self.initiateMenues()
       
    def createRoomMenu(self):
        """

    	Creates the enu to be used with a list of available rooms

    	Key arguments:
    		master - The in which to display the menu
    		socket - The socket to send messages to
    	"""
        
        menu = tki.Menu(self.master,tearoff=0)
        menu.add_command(label = "Join", command = self.join)
        menu.add_command(label = "Close Menu", command = menu.unpost)
        return menu

##############################################################################################

    def createUserMenu(self):
        """

    	Creates the menu to be used for a list of users

    	Key arguments:
    		master - The frame to display the menu in
    		socket - The socket to send messages to
    		userName - The username of the user
    	"""
     
        menu = tki.Menu(self.master,tearoff=0)
        menu.add_command(label = "Whisper", command = self.whisper)
        menu.add_command(label = "Whois",command = self.whois)
        menu.add_command(label = "Track",command = self.track)
        menu.add_command(label = "Dummy",command = menu.unpost)
        menu.add_command(label = "Dummy",command = menu.unpost)
        return menu

    def updateRoomList(self,roomList):
        """
	
	Creates the list of rooms to be used with the invite option
	
	Key arguments:
		roomList - A list of the rooms the user is a member of
	Sideeffect:
		userMenu updated with the new submenu
	"""

        roomMenu = tki.Menu(self.userMenu,tearoff=0)
        
        def updateRoomListAux(menu,text):
            menu.add_command(label=text,command = lambda: self.invite(text))

        for roomName in roomList:
            updateRoomListAux(roomMenu,roomName)
        
        self.userMenu.delete(3,5)
        self.userMenu.add_cascade(label="Invite", menu=roomMenu)
        self.userMenu.add_command(label = "Close",command = self.userMenu.unpost)

    def invite(self,roomName):
        """

	Invites another user to a room the user is a member of

	Key arguments: 
		user - The name of the user to invite
		roomName - The room to invite to
        """

        self.sendCommand(roomName +" /invite",self.current)

    def whisper(self):
        """

	Starts a private conversation with another user

	Key arguments: 
		name - The name of the other user to whisper to
	Side effects:
		A new private room is created	
        """
        print("Current är: " + self.current)
        self.sendCommand("global /join",self.current+self.userName+" private")
        self.master.after(300,self.sendCommand,self.current+self.userName+" /invite",self.current)
	

    def whois(self):

        """

        Sends a request to the server for information where a user
        is connected from

        Key arguments:
        	name - The user to request information about
        """
        self.sendCommand("global /whois",self.current)
        
    def track(self):
        """

    	Sends a request to the server for all the rooms a user is a member of

    	Key arguments:
    		name - The user to request information about
    	"""
        self.sendCommand("global /track",self.current)

    def setRoomList(self,roomList):
        """

    	Updates the list of which rooms the user is currently a member of

    	Key arguments:
    		roomList - A new list of which the user is currently a member of
    	"""
        self.roomList = roomList

    def setName(self,newName):
        """

    	Changes the users username

    	Key arguments:
    		newName - The new username
    	"""
        self.userName = newName

    def join(self):
        """

    	Sends a request to join the selected room

    	Key arguments:
    		name - The name of the room to join
    	"""
        
        if self.current in self.roomList:
            pass
        else:
            self.sendCommand("global /join",self.current)
        
    def popup(self,menu,event):
        """

    	Displays the meny

    	Key arguments:
    		event - The user right clicks
    	"""
        
        self.meny.post(event.x_root, event.y_root)
        self.roomMenu.post(event.x_root, event.y_root)

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

####################################################################################### 
        
    def initiateMenues(self):
        """

        Creates userMenu and roomMenu that appears when the user right-clicks.
        """
        self.userMenu = self.createUserMenu()
        self.userWindow.bind('<Button-3>',lambda e: self.setAndPopup(e,self.userWindow))
        self.updateRoomList(self.roomList)

        self.roomMenu = self.createRoomMenu()
        self.roomWindow.bind('<Button-3>',lambda e: self.setAndPopup(e,self.roomWindow))

####################################################################################### 

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
            self.current = listbox.get(ACTIVE)
            self.userMenu.post(event.x_root, event.y_root)
        else:
            self.current = listbox.get(ACTIVE)
            self.roomMenu.post(event.x_root, event.y_root)
