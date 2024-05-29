package com.rajalastudios.roboterfrontend.ui.fragments;

import android.content.Context;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rajalastudios.roboterfrontend.MainActivity;
import com.rajalastudios.roboterfrontend.R;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

public class HomeFragment extends Fragment {
    private Map<String, String> settings;
    private Map<String, Boolean> booleanCache;
    private MainActivity mainActivity;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_home, container, false);
        mainActivity = (MainActivity) getActivity();
        TextView connection_status = view.findViewById(R.id.connection_status);

        settings = mainActivity.settings;

        mainActivity.connectForTrust();

        loadOrGenerateBoolCache();
        booleanCache = mainActivity.boolCache;

        if (!(booleanCache == null)) {
            Log.d("DEBUG", "booleanCache(connected) is: " + booleanCache.get("connected"));
        } else {
            Log.d("DEBUG", "booleanCache is null");
        }

        if (Boolean.TRUE.equals(mainActivity.boolCache.get("connected"))) connection_status.setText(R.string.connected);
        else if (Boolean.FALSE.equals(mainActivity.boolCache.get("connected"))) connection_status.setText(R.string.disconnected);

        return view;
    }


    public void loadOrGenerateBoolCache() {
        try {
            booleanCache = mainActivity.boolCache;
        } catch (NullPointerException e) {
            e.printStackTrace();
            booleanCache = new HashMap<>();
        }
        if (booleanCache == null) {
            booleanCache = new HashMap<>();
            booleanCache.put("connected", false);
            saveBoolCache();
        }
    }

    private void saveBoolCache() {
        mainActivity.boolCache = booleanCache;
    }
}