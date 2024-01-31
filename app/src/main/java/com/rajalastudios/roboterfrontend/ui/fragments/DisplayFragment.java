package com.rajalastudios.roboterfrontend.ui.fragments;

import android.media.MediaPlayer;
import android.net.Uri;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.VideoView;

import com.rajalastudios.roboterfrontend.R;

public class DisplayFragment extends Fragment {
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_display, container, false);

        VideoView videoView = view.findViewById(R.id.never_gonna_give_you_up);
        Uri uri = Uri.parse("android.resource://" + getActivity().getPackageName() + "/" + R.raw.nggyu);
        videoView.setVideoURI(uri);
        videoView.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
            @Override
            public void onPrepared(MediaPlayer mp) {
                videoView.start();
            }
        });

        return view;
    }
}