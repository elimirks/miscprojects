// https://gitlab.freedesktop.org/gstreamer/gstreamer-rs/-/tree/main/tutorials/src
use anyhow::Context;

use gstreamer::{self as gst, ElementFactory, GhostPad, PadTemplate, State};
use gstreamer::prelude::*;

pub type AnyRes<T> = anyhow::Result<T>;

fn create_mux_av_combine_bin(
    video_source: gst::Element,
    audio_source: gst::Element,
) -> AnyRes<gst::Bin> {
    let bin = gst::Bin::new();
    let audio_demuxer = ElementFactory::make("matroskademux")
        .build()?;
    // TODO: also support MP4 - use a signal
    let video_demuxer = ElementFactory::make("matroskademux")
        .build()?;
    let mux = ElementFactory::make("webmmux")
        .build()?;

    bin.add_many([
        &video_source,
        &audio_source,
        &video_demuxer,
        &audio_demuxer,
        &mux,
    ])?;

    // TODO: use streamsynchronizer for these
    video_source.link(&video_demuxer)?;
    audio_source.link(&audio_demuxer)?;

    let cloned_mux = mux.clone();
    video_demuxer.connect_pad_added(move |demuxer, pad| {
        if pad.name().starts_with("video") {
            // TODO:post errors
            demuxer.link(&cloned_mux).unwrap();
        }
    });

    let cloned_mux = mux.clone();
    audio_demuxer.connect_pad_added(move |demuxer, pad| {
        if pad.name().starts_with("audio") {
            demuxer.link(&cloned_mux).unwrap();
        }
    });

    let out_pad = mux.static_pad("src").context("Expected src pad")?;
    let ghost_pad = GhostPad::with_target(&out_pad)?;
    bin.add_pad(&ghost_pad)?;
    Ok(bin)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    gst::init()?;
    if let Err(err) = glue() {
        eprintln!("Error: {:?}", err);
    }
    Ok(())
}

fn glue() -> AnyRes<()> {
    let av_mux_bin_1 = create_mux_av_combine_bin(
        ElementFactory::make("filesrc")
            .property("location", "data/video_244ef87e-000a-47f6-9a49-04f4fa93f8f2.vp9.webm")
            .build()?,
        ElementFactory::make("filesrc")
            .property("location", "data/audio_244ef87e-000a-47f6-9a49-04f4fa93f8f2.opus.webm")
            .build()?,
    )?;

    let av_mux_bin_2 = create_mux_av_combine_bin(
        ElementFactory::make("filesrc")
            .property("location", "data/video_604370e8-3cac-4633-b11a-d539f4f537d1.vp9.webm")
            .build()?,
        ElementFactory::make("filesrc")
            .property("location", "data/audio_604370e8-3cac-4633-b11a-d539f4f537d1.opus.webm")
            .build()?,
    )?;

    let concat = ElementFactory::make("concat").build()?;

    // Save to file
    let file_sink = ElementFactory::make("filesink")
        .property("location", "../output.webm")
        .build()?;

    let pipeline = gst::Pipeline::default();
    pipeline.add(&av_mux_bin_1)?;
    pipeline.add(&av_mux_bin_2)?;
    pipeline.add(&concat)?;
    pipeline.add(&file_sink)?;

    // TODO: pad template to auto-gen the number?
    concat.request_pad_simple("sink_0").unwrap();
    concat.request_pad_simple("sink_1").unwrap();
    av_mux_bin_1.link_pads(None, &concat, Some("sink_0"))?;
    av_mux_bin_2.link_pads(None, &concat, Some("sink_1"))?;
    // concat.request_pad_simple("sink_4").unwrap();
    // av_mux_bin_1.link_pads(None, &concat, Some("sink_0"))?;
    // concat.request_pad_simple("sink_1").unwrap();
    // av_mux_bin_2.link_pads(None, &concat, Some("sink_1"))?;

    concat.link(&file_sink)?;

    let bus = pipeline.bus().context("no bus")?;

    println!("{}", pipeline.debug_to_dot_data(gst::DebugGraphDetails::ALL));
    pipeline.set_state(State::Playing)?;
    for msg in bus.iter_timed(gst::ClockTime::NONE) {
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
