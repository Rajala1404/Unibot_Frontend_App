package com.rajalastudios.roboterfrontend;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import android.content.Context;
import android.os.Bundle;
import android.util.Log;
import android.view.MenuItem;
import android.widget.TextView;

import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.rajalastudios.roboterfrontend.ui.fragments.ControllerFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.DisplayFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.HomeFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.LogsFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.SettingsFragment;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketTimeoutException;
import java.util.*;

public class MainActivity extends AppCompatActivity {
    public Map<String, String> settings = new HashMap<>();
    public Map<String, Boolean> boolCache = new HashMap<>();

    public int portFirstAck = 6001;
    public int portAckConnected = 6002;

    private boolean sendTrustData_lock = false;
    private boolean connectionTest_lock = false;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        loadMapFromFile("settings.ludat");
        BottomNavigationView mBottomNavigationView = (BottomNavigationView) findViewById(R.id.bottom_nav_view);

        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setChecked(true);
        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setIcon(R.drawable.round_home_24);
        loadFragment(new HomeFragment(), true);
        mBottomNavigationView.setOnNavigationItemSelectedListener( new BottomNavigationView.OnNavigationItemSelectedListener() {
            @Override
            public boolean onNavigationItemSelected(@NonNull MenuItem item) {
                int itemId = item.getItemId();

                if (itemId == R.id.nav_display) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_smart_display_24);
                    loadFragment(new DisplayFragment(), false);
                } else if (itemId == R.id.nav_controller) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_settings_remote_24);
                    loadFragment(new ControllerFragment(), false);
                } else if (itemId == R.id.nav_home) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_home_24);
                    loadFragment(new HomeFragment(), false);
                } else if (itemId == R.id.nav_logs) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_assignment_24);
                    loadFragment(new LogsFragment(), false);
                } else if (itemId == R.id.nav_settings) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_settings_24);
                    loadFragment(new SettingsFragment(), false);
                }
                return true;
            }
        });

        loadFragment(new HomeFragment(), false);
    }

    public void sendData(String s) {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(boolCache.get("connected"))) {
                    DatagramPacket sendPacket;
                    DatagramSocket clientSocket = null;
                    byte[] sendData;
                    try {
                        Log.d("MainActivity.sendData", "Trying to send: " + s);
                        clientSocket = new DatagramSocket();
                        clientSocket.setSoTimeout(1000);
                        sendData = s.getBytes();
                        sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(settings.get("ipAddress")), Integer.parseInt(Objects.requireNonNull(settings.get("port"))));
                        clientSocket.send(sendPacket);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    assert clientSocket != null;
                    if (clientSocket.isBound()) clientSocket.close();
                }
            }
        });
        thread.start();
    }

    private void loadFragment(Fragment fragment, boolean isAppInitD) {
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();

        if (isAppInitD) {
            fragmentTransaction.add(R.id.frame_layout, fragment);
        } else {
            fragmentTransaction.replace(R.id.frame_layout, fragment);
        }

        fragmentTransaction.replace(R.id.frame_layout, fragment);
        fragmentTransaction.commit();
    }

    private void setNavBarIconsToDefault() {
        BottomNavigationView mBottomNavigationView = findViewById(R.id.bottom_nav_view);
        mBottomNavigationView.getMenu().findItem(R.id.nav_display).setIcon(R.drawable.outline_smart_display_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_controller).setIcon(R.drawable.outline_settings_remote_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setIcon(R.drawable.outline_home_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_logs).setIcon(R.drawable.outline_assignment_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_settings).setIcon(R.drawable.outline_settings_24);
    }


    public void saveMapToFile(Map<String, String> map, String fileName) {
        try {
            FileOutputStream fileOutputStream = openFileOutput(fileName, Context.MODE_PRIVATE);
            ObjectOutputStream objectOutputStream = new ObjectOutputStream(fileOutputStream);
            objectOutputStream.writeObject(map);
            objectOutputStream.close();
            fileOutputStream.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Map<String, String> loadMapFromFile(String fileName) {
        Map<String, String> map = null;
        try {
            FileInputStream fileInputStream = openFileInput(fileName);
            ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream);
            map = (Map<String, String>) objectInputStream.readObject();
            objectInputStream.close();
            fileInputStream.close();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
        return map;
    }

    public void saveSettings(){
        saveMapToFile(settings, "settings.ludat");
    }

    private void sendTrustData(final String value) {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                sendTrustData_lock = true;
                Log.i("INFO", "Trying to send...");
                DatagramPacket sendPacket;
                DatagramSocket clientSocket = null;
                boolean success = false;
                byte[] sendData;
                try {
                    clientSocket = new DatagramSocket();
                    clientSocket.setSoTimeout(1000);
                    sendData = value.getBytes();
                    sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(settings.get("ipAddress")), Integer.parseInt(Objects.requireNonNull(settings.get("port"))));
                    clientSocket.send(sendPacket);
                    if (clientSocket.isBound()) clientSocket.close();

                    clientSocket = new DatagramSocket(portFirstAck);
                    clientSocket.setSoTimeout(30000);
                    byte[] ackBuffer = new byte[1024];
                    DatagramPacket ackPacket = new DatagramPacket(ackBuffer, ackBuffer.length);
                    try {
                        try {
                            clientSocket.receive(ackPacket);
                            String ackMessage = new String(ackPacket.getData(),0,ackPacket.getLength());
                            Log.i("NetworkTask", "ACK received");
                            Log.d("NetworkTask", "ACK MESSAGE IS: " + ackMessage);
                            if (ackMessage.equals("true")) {
                                Log.i("NetworkTask", "Trust received");
                                connectionTest();
                            } else {
                                Exception e = new Exception();
                                throw e;
                            }
                            success = true;
                            boolCache.put("connected", true);
                            MainActivity.this.runOnUiThread(new Runnable() {
                                @Override
                                public void run() {
                                    TextView connectionText = (TextView) findViewById(R.id.connection_status);
                                    if (connectionText != null) {
                                        connectionText.setText(getString(R.string.connected));
                                    }
                                }
                            });
                        } catch (SocketTimeoutException e) {
                            boolCache.put("connected", false);
                            MainActivity.this.runOnUiThread(new Runnable() {
                                @Override
                                public void run() {
                                    TextView connectionText = (TextView) findViewById(R.id.connection_status);
                                    if (connectionText != null) {
                                        connectionText.setText(getString(R.string.disconnected));
                                    }
                                }
                            });
                            Log.e("NetworkTask", "Connection Failed: ACK timeout or Trust was denied");
                        }
                    } catch (Exception e) {
                        Log.e("NetworkTask", "Error sending data: " + e.getMessage());
                    }
                } catch (Exception e) {
                    Log.e("NetworkTask", "Error sending data: " + e.getMessage());
                } finally {
                    if (success) {
                        Log.i("NetworkTask", "Successfully Connected!");
                    }
                }
                if (clientSocket != null) {
                    clientSocket.close();
                }
                sendTrustData_lock = false;
            }
        });
        if (!(thread.isAlive()) && Boolean.FALSE.equals(boolCache.get("connected")) && !sendTrustData_lock) thread.start();
    }

    private void connectionTest() {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                connectionTest_lock = true;
                Log.i("INFO", "Starting Testing Connection");
                DatagramPacket sendPacket;
                DatagramSocket clientSocket = null;
                String value = "CONNECTED";
                int fails = 0;
                byte[] sendData;


                while (true) {
                    try {
                        Thread.sleep(5000);
                    } catch (InterruptedException e) {
                        Log.e("Thread Sleep", "Thread Interrupted");
                    }
                    if (fails >= 3) {
                        Log.i("NetworkTask", "Closing Socket");
                        break;
                    }
                    try {
                        clientSocket = new DatagramSocket();
                    } catch (Exception e) {
                        Log.e("NetworkTask", "Failed to connect: " + e.getMessage());
                    }
                    try {
                        clientSocket.setSoTimeout(1000);
                        sendData = value.getBytes();
                        sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(settings.get("ipAddress")), Integer.parseInt(settings.get("port")));
                        clientSocket.send(sendPacket);
                    } catch (Exception e) {
                        Log.e("NetworkTask/connectionTest", "Connection Failed: " + e.getMessage());
                    } finally {
                        if (clientSocket.isBound()) clientSocket.close();
                    }
                    byte[] ackBuffer = new byte[1024];
                    DatagramPacket ackPacket = new DatagramPacket(ackBuffer, ackBuffer.length);
                    try {
                        clientSocket = new DatagramSocket(portAckConnected);
                        clientSocket.setSoTimeout(1000);
                    } catch (Exception e) {
                        Log.e("NetworkTask/connectionTest", "Failed to open ACK Listener: ");
                    }
                    try {
                        clientSocket.receive(ackPacket);
                        boolCache.put("connected", true);
                        MainActivity.this.runOnUiThread(new Runnable() {
                            @Override
                            public void run() {
                                TextView connectionText = (TextView) findViewById(R.id.connection_status);
                                if (connectionText != null) {
                                    connectionText.setText(getString(R.string.connected));
                                }
                            }
                        });
                    } catch (Exception e) {
                        boolCache.put("connected", false);
                        MainActivity.this.runOnUiThread(new Runnable() {
                            @Override
                            public void run() {
                                TextView connectionText = (TextView) findViewById(R.id.connection_status);
                                if (connectionText != null) {
                                    connectionText.setText(getString(R.string.disconnected));
                                }
                            }
                        });
                        fails++;
                        Log.e("NetworkTask", "Connection Failed: ACK timeout");
                    }
                    if (clientSocket.isBound()) clientSocket.close();
                    connectionTest_lock = false;
                }
                if (!(clientSocket == null)) clientSocket.close();
            }

        });
        if (!(thread.isAlive()) && !connectionTest_lock) thread.start();
    }

    public void connectForTrust() {
        if (!(settings == null) && !(Objects.equals(settings.get("ipAddress"), "") && Objects.equals(settings.get("port"), ""))) {
            try {
                sendTrustData("TRUST");
            } catch (Exception e) {
                e.printStackTrace();
                Log.d("ERROR", "Connection Failed!");
                boolCache.put("connected", false);
            } finally {
                boolCache.putIfAbsent("connected", false);
            }
        }
    }
}