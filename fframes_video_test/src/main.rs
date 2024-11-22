use std::ops::DerefMut;
use std::thread::sleep_ms;

// https://gitlab.freedesktop.org/gstreamer/gstreamer-rs/-/tree/main/tutorials/src
use anyhow::Context;

use gstreamer::{self as gst, DebugGraphDetails, ElementFactory, GhostPad, MessageView, State};
use gstreamer::prelude::*;

pub type AnyRes<T> = anyhow::Result<T>;

fn create_demux_bin(
    video_source: gst::Element,
    audio_source: gst::Element,
) -> AnyRes<gst::Bin> {
    let bin = gst::Bin::new();

    // Ghost are like symbolic links to other pads inside this bin
    // They let us abstract away all the dynamic details internally
    let video_ghost_pad = GhostPad::new(gstreamer::PadDirection::Src);
    video_ghost_pad.set_property("name", "video");
    bin.add_pad(&video_ghost_pad)?;

    let audio_ghost_pad = GhostPad::new(gstreamer::PadDirection::Src);
    audio_ghost_pad.set_property("name", "audio");
    bin.add_pad(&audio_ghost_pad)?;

    let audio_demuxer = ElementFactory::make("matroskademux")
        .build()?;
    // TODO: also support MP4 - use a signal
    let video_demuxer = ElementFactory::make("matroskademux")
        .build()?;

    let stream_sync = ElementFactory::make("streamsynchronizer")
        .build()?;

    bin.add_many([
        &video_source,
        &audio_source,
        &video_demuxer,
        &audio_demuxer,
        &stream_sync,
    ])?;

    video_source.link(&video_demuxer)?;
    audio_source.link(&audio_demuxer)?;

    // Ensure the group IDs of the video & audio data are the same, so that
    // stream synchronizers won't deadlock
    let group_id = gst::GroupId::next();
    let set_group_id_probe = move |pad: &gst::Pad, probe_info: &mut gst::PadProbeInfo| {
        let Some(event) = probe_info.event_mut() else {
            return gst::PadProbeReturn::Ok;
        };
        let binding: &mut gst::EventRef = event.make_mut();
        match binding.view_mut() {
            gst::EventViewMut::Eos(event) => {
                println!("eos for {:?}: {:?}", pad.name(), event);
            }
            gst::EventViewMut::StreamStart(event) => {
                println!("Setting group id to {group_id:?}");
                println!("Stream start event: {:?}", event);
                event.set_group_id(group_id);
            }
            _ => {}
        }
        gst::PadProbeReturn::Ok
    };

    let video_sync_sink = stream_sync
        .request_pad_simple("sink_0")
        .context("failed creating stream sync sink")?;
    let audio_sync_sink = stream_sync
        .request_pad_simple("sink_1")
        .context("failed creating stream sync sink")?;

    let video_sync_src = stream_sync
        .static_pad("src_0")
        .context("failed getting stream sync src")?;
    let audio_sync_src = stream_sync
        .static_pad("src_1")
        .context("failed getting stream sync src")?;

    video_demuxer.connect_pad_added(move |demuxer, pad| {
        if pad.name().starts_with("video") {
            // demuxer.send_event(event) TODO: in case of errors
            println!("Settings target for video ghost pad");
            pad.link(&video_sync_sink).unwrap();
            pad.add_probe(gst::PadProbeType::EVENT_DOWNSTREAM, set_group_id_probe);
        }
    });

    audio_demuxer.connect_pad_added(move |demuxer, pad| {
        if pad.name().starts_with("audio") {
            println!("Settings target for audio ghost pad");
            if let Err(err) = pad.link(&audio_sync_sink) {
                panic!("Error linking audio pad: {:?}, {:?}", err, err.to_string());
            }
            pad.add_probe(gst::PadProbeType::EVENT_DOWNSTREAM, set_group_id_probe);
        }
    });

    video_ghost_pad.set_target(Some(&video_sync_src))?;
    audio_ghost_pad.set_target(Some(&audio_sync_src))?;

    Ok(bin)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    gst::init()?;
    if let Err(err) = test_demux_bin_then_mux() {
        eprintln!("Error: {:?}", err);
    }
    Ok(())
}

fn test_demux_bin_fakesink() -> AnyRes<()> {
    let demux_bin = create_demux_bin(
        ElementFactory::make("filesrc")
            .property("location", "data/video_244ef87e-000a-47f6-9a49-04f4fa93f8f2.vp9.webm")
            .build()?,
        ElementFactory::make("filesrc")
            .property("location", "data/audio_244ef87e-000a-47f6-9a49-04f4fa93f8f2.opus.webm")
            .build()?,
    )?;

    let fake_sink_0 = ElementFactory::make("fakesink").build()?;
    let fake_sink_1 = ElementFactory::make("fakesink").build()?;

    let pipeline = gst::Pipeline::default();
    pipeline.add_many([&fake_sink_0, &fake_sink_1])?;
    pipeline.add_many([&demux_bin])?;

    demux_bin.link_pads(Some("audio"), &fake_sink_0, None)?;
    demux_bin.link_pads(Some("video"), &fake_sink_1, None)?;

    let bus = pipeline.bus().context("no bus")?;
    pipeline.set_state(State::Playing)?;

    for msg in bus.iter_timed(gst::ClockTime::NONE) {
        println!("{}", pretty_fmt_message(&msg.view())?);

        use gst::MessageView;

        match msg.view() {
            MessageView::Eos(..) => break,
            MessageView::Error(err) => {
                println!("err: {}", err.to_string());
                return Ok(());
            }
            _ => {}
        }
    }
    pipeline.set_state(State::Null)?;

    Ok(())
}

fn test_demux_bin_then_mux() -> AnyRes<()> {
    let demux_bin = create_demux_bin(
        ElementFactory::make("filesrc")
            .property("location", "data/video_244ef87e-000a-47f6-9a49-04f4fa93f8f2.vp9.webm")
            .build()?,
        ElementFactory::make("filesrc")
            .property("location", "data/audio_244ef87e-000a-47f6-9a49-04f4fa93f8f2.opus.webm")
            .build()?,
    )?;

    let fake_sink = ElementFactory::make("fakesink").build()?;
    let mux = ElementFactory::make("webmmux").build()?;
    let file_sink = ElementFactory::make("filesink")
        .property("location", "../output.webm")
        .build()?;

    let pipeline = gst::Pipeline::default();
    pipeline.add_many([&fake_sink, &mux, &file_sink])?;
    pipeline.add_many([&demux_bin])?;

    // FIXME: One or the other works, but not both together...
    // Why!?
    // Maybe a frame rate mismatch or something
    // Try out `audiorate` and `videorate` filters
    demux_bin.link_pads(Some("audio"), &mux, Some("audio_0"))?;
    demux_bin.link_pads(Some("video"), &fake_sink, None)?;
    mux.link(&file_sink)?;

    let bus = pipeline.bus().context("no bus")?;
    pipeline.set_state(State::Playing)?;

    for msg in bus.iter_timed(gst::ClockTime::NONE) {
        println!("{}", pretty_fmt_message(&msg.view())?);

        use gst::MessageView;

        match msg.view() {
            MessageView::Eos(..) => break,
            MessageView::Error(err) => {
                println!("err: {}", err.to_string());
                return Ok(());
            }
            _ => {}
        }
    }
    pipeline.set_state(State::Null)?;

    Ok(())
}

fn glue() -> AnyRes<()> {
    let demux_bin_1 = create_demux_bin(
        ElementFactory::make("filesrc")
            .property("location", "data/video_244ef87e-000a-47f6-9a49-04f4fa93f8f2.vp9.webm")
            .build()?,
        ElementFactory::make("filesrc")
            .property("location", "data/audio_244ef87e-000a-47f6-9a49-04f4fa93f8f2.opus.webm")
            .build()?,
    )?;

    let demux_bin_2 = create_demux_bin(
        ElementFactory::make("filesrc")
            .property("location", "data/video_604370e8-3cac-4633-b11a-d539f4f537d1.vp9.webm")
            .build()?,
        ElementFactory::make("filesrc")
            .property("location", "data/audio_604370e8-3cac-4633-b11a-d539f4f537d1.opus.webm")
            .build()?,
    )?;

    let concat_audio = ElementFactory::make("concat").build()?;
    let concat_video = ElementFactory::make("concat").build()?;

    let mux = ElementFactory::make("webmmux").build()?;

    // Save to file
    let file_sink = ElementFactory::make("filesink")
        .property("location", "../output.webm")
        .build()?;

    let pipeline = gst::Pipeline::default();
    pipeline.add_many([&demux_bin_1, &demux_bin_2])?;
    pipeline.add_many([&concat_audio, &concat_video, &mux, &file_sink])?;

    // TODO: PadTemplate to auto-gen the number?
    concat_audio.request_pad_simple("sink_0").context("failed requesting pad")?;
    concat_audio.request_pad_simple("sink_1").context("failed requesting pad")?;

    concat_video.request_pad_simple("sink_0").context("failed requesting pad")?;
    concat_video.request_pad_simple("sink_1").context("failed requesting pad")?;

    // TODO: add a synchronize stream before concat

    demux_bin_1.link_pads(Some("audio"), &concat_audio, Some("sink_0"))?;
    demux_bin_2.link_pads(Some("audio"), &concat_audio, Some("sink_1"))?;

    demux_bin_1.link_pads(Some("video"), &concat_video, Some("sink_0"))?;
    demux_bin_2.link_pads(Some("video"), &concat_video, Some("sink_1"))?;

    concat_audio.link_pads(Some("src"), &mux, Some("audio_0"))?;
    concat_video.link_pads(Some("src"), &mux, Some("video_0"))?;

    // TODO: Figure out why this mux is pausing
    mux.link(&file_sink)?;

    let bus = pipeline.bus().context("no bus")?;

    println!("{}", pipeline.debug_to_dot_data(gst::DebugGraphDetails::ALL));
    pipeline.set_state(State::Playing)?;
    println!("Start!");
    for msg in bus.iter_timed(gst::ClockTime::NONE) {
        println!("{}", pretty_fmt_message(&msg.view())?);

        use gst::MessageView;

        match msg.view() {
            MessageView::Eos(..) => break,
            MessageView::Error(err) => {
                println!("err: {}", err.to_string());
                return Ok(());
            }
            _ => {}
        }
    }
    //sleep_ms(10000);
    // println!("{}", pipeline.debug_to_dot_data(gst::DebugGraphDetails::ALL));
    // println!("Done?");
    pipeline.set_state(State::Null)?;

    Ok(())
}

fn pretty_fmt_state(state: &State) -> &'static str {
    match state {
        State::VoidPending => "Void",
        State::Null => "Null",
        State::Ready => "Ready",
        State::Paused => "Paused",
        State::Playing => "Playing",
    }
}

fn pretty_fmt_message(message: &MessageView) -> AnyRes<String> {
    Ok(match message {
        MessageView::Eos(_) => "EOS".to_string(),
        MessageView::Error(err) => format!("ERROR: {}", err.to_string()),
        MessageView::StateChanged(state) => {
            let source = state.src().context("expected state source")?;
            if state.pending() == State::VoidPending {
                format!(
                    "State change for {:<16} {:<7} -> {:<7}",
                    source.name().as_str(),
                    pretty_fmt_state(&state.old()),
                    pretty_fmt_state(&state.current()),
                )
            } else {
                format!(
                    "State change for {:<16} {:<7} -> {:<7} pending: {:<7}",
                    source.name().as_str(),
                    pretty_fmt_state(&state.old()),
                    pretty_fmt_state(&state.current()),
                    pretty_fmt_state(&state.pending()),
                )
            }
        }
        MessageView::StreamStatus(status) => {
            let structure = status.structure().context("expected stream status structure")?;
            let _type: gst::StreamStatusType = structure.get("type")?;
            let task: gst::Task = structure.get("object")?;
            format!("StreamStatus {:?}: {:?}", task.name(), _type)
        }
        _ => format!("{:?}", message)
    })
}
