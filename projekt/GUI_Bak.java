import java.io.IOException;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


public class GUI_Bak extends javax.swing.JFrame {

    public GUI_Bak() {
        initComponents();
    }

    
    @SuppressWarnings("unchecked")
                           
    private void initComponents() {
         
        jScrollPane1 = new javax.swing.JScrollPane();
        ChatHistory = new javax.swing.JTextArea();
        Message = new javax.swing.JTextField();
        SendButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        ChatHistory.setColumns(20);
        ChatHistory.setRows(5);
        jScrollPane1.setViewportView(ChatHistory);

        SendButton.setText("Send");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1)
            .addComponent(Message)
            .addGroup(layout.createSequentialGroup()
                .addGap(222, 222, 222)
                .addComponent(SendButton)
                .addContainerGap(202, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 432, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Message, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 35, Short.MAX_VALUE)
                .addComponent(SendButton))
        );
        //***************************************************************
        //Actionlisteners och initieringar
        //***************************************************************
        ChatHistory.setEditable(false);
        
        Message.addActionListener(new ActionListener(){

               public void actionPerformed(ActionEvent e){
                      String temp = Message.getText();
                      if (!(temp.equals(""))){
                          ChatHistory.append(temp+ '\n');
                          Message.setText("");
                        }
               }});
               
        SendButton.addActionListener(new ActionListener(){

               public void actionPerformed(ActionEvent e){
                      String temp = Message.getText();
                      if (!(temp.equals(""))){
                          ChatHistory.append(temp+ '\n');
                          Message.setText("");
                        }
               }});
        //***************************************************************
        //Slut Actionlisteners och initieringar
        //***************************************************************
        
        pack();
    }                       

  
    public static void main(String args[]) {
       
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(GUI_Bak.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(GUI_Bak.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(GUI_Bak.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(GUI_Bak.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
       
        
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new GUI_Bak().setVisible(true);
            }
        });
    }
    
    static String server = "server";
    private javax.swing.JTextArea ChatHistory;
    private javax.swing.JTextField Message;
    private javax.swing.JButton SendButton;
    private javax.swing.JScrollPane jScrollPane1;
}
