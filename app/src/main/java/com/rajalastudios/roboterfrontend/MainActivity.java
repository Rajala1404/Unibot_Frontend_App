package com.rajalastudios.roboterfrontend;

import static com.google.android.material.internal.ContextUtils.getActivity;

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
import android.widget.Button;
import android.widget.TextView;

import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.rajalastudios.roboterfrontend.ui.fragments.ControllerFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.DisplayFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.HomeFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.LogsFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.SettingsFragment;

import java.io.*;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.util.*;

public class MainActivity extends AppCompatActivity {
    private Button connectButton;
    public Map<String, String> settings = new HashMap<>();
    public Map<String, Boolean> boolCache = new HashMap<>();

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        settings = loadMapFromFile("settings.ludat");
        BottomNavigationView mBottomNavigationView = (BottomNavigationView) findViewById(R.id.bottom_nav_view);

        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setChecked(true);
        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setIcon(R.drawable.round_home_24);
        loadFragment(new HomeFragment(), true);
        mBottomNavigationView.setOnNavigationItemSelectedListener(new BottomNavigationView.OnNavigationItemSelectedListener() {
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
        BottomNavigationView mBottomNavigationView = (BottomNavigationView) findViewById(R.id.bottom_nav_view);
        mBottomNavigationView.getMenu().findItem(R.id.nav_display).setIcon(R.drawable.outline_smart_display_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_controller).setIcon(R.drawable.outline_settings_remote_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setIcon(R.drawable.outline_home_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_logs).setIcon(R.drawable.outline_assignment_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_settings).setIcon(R.drawable.outline_settings_24);
    }

    public void saveSettings(){
        saveMapToFile(settings, "settings.ludat");
    }

    private void sendTrustData(final String value) throws InterruptedException {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                Log.i("INFO", "Trying to send...");
                DatagramPacket sendPacket;
                DatagramSocket clientSocket = null;
                Boolean success = false;
                byte[] sendData;
                try {
                    clientSocket = new DatagramSocket(Integer.parseInt(settings.get("port")));
                    clientSocket.setSoTimeout(1000);
                    sendData = value.getBytes();
                    sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(settings.get("ipAddress")), Integer.parseInt(settings.get("port")));
                    clientSocket.send(sendPacket);

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
                        } catch (SocketTimeoutException e) {
                            boolCache.put("connected", false);
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
                    if (clientSocket != null) {
                        clientSocket.close();
                    }
                }
            }
        });
        if (!(thread.isAlive())) thread.start();
    }

    private void connectionTest() {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                Log.i("INFO", "Starting Testing Connection");
                DatagramPacket sendPacket;
                DatagramSocket clientSocket = null;
                Boolean success = false;
                String value = "CONNECTED";
                Integer fails = 0;
                byte[] sendData;
                try {
                    clientSocket = new DatagramSocket(Integer.parseInt(settings.get("port")));
                    clientSocket.setSoTimeout(1000);
                    sendData = value.getBytes();
                    sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(settings.get("ipAddress")), Integer.parseInt(settings.get("port")));
                    clientSocket.send(sendPacket);
                } catch (Exception e) {
                    Log.e("NetworkTask", "Connection Failed: " + e.getMessage());
                }

                try {
                    clientSocket = new DatagramSocket(Integer.parseInt(settings.get("port"))+1);
                } catch (Exception e) {
                    Log.e("NetworkTask", "Connection Failed: " + e.getMessage());
                }
                while (true) {
                    try {
                        Thread.sleep(3000);
                    } catch (InterruptedException e) {
                        Log.e("Thread Sleep", "Thread Interrupted");
                    }
                    if (fails >= 3) {
                        break;
                    }
                    try {
                        clientSocket.setSoTimeout(1000);
                        sendData = value.getBytes();
                        sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(settings.get("ipAddress")), Integer.parseInt(settings.get("port")));
                        clientSocket.send(sendPacket);
                    } catch (Exception e) {
                        Log.e("NetworkTask", "Connection Failed: " + e.getMessage());
                    }
                    byte[] ackBuffer = new byte[1024];
                    DatagramPacket ackPacket = new DatagramPacket(ackBuffer, ackBuffer.length);
                    try {
                        clientSocket.receive(ackPacket);
                        boolCache.put("connected", true);
                    } catch (Exception e) {
                        boolCache.put("connected", false);
                        fails++;
                        Log.e("NetworkTask", "Connection Failed: ACK timeout");
                    }
                }
            }

        });
        if (!(thread.isAlive())) thread.start();
    }

    public void connectForTrust(TextView connectedText) {
        if (!(settings == null) && !(settings.get("ipAddress") == "" && settings.get("port") == "")) {
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
}