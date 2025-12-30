import sys
try:
    import cv2
    from os import path, makedirs
    from tqdm import tqdm
    import yaml
    from glob import glob
    import traceback
except Exception as e:
    e = str(e)
    start = e.index("'") + 1
    stop = e.index("'", start)
    library = e[start:stop]
    sys.stderr.write(f"[ERROR] You need to install the {library} library.\n")
    sys.exit(1)


def stem(file_path):
    return path.splitext(path.basename(file_path))[0]

def reorient(frame, rotate=0, v_flip=False, h_flip=False):
    if rotate % 360 != 0:
        frame = cv2.rotate(frame, rotate)
    if v_flip:
        frame = cv2.flip(frame, 0)
    if h_flip:
        frame = cv2.flip(frame, 1)
    return frame

def makedir(dir_path):
    if not path.exists(dir_path):
        parent_dir = path.dirname(dir_path)
        if parent_dir != '' and not path.exists(parent_dir):
            raise OSError(f"Directory '{parent_dir}' does not exist.")
        makedirs(dir_path)

def get_frame(video_stream, frame_number):
    video_stream.set(cv2.CAP_PROP_POS_FRAMES, frame_number)
    status, frame = video_stream.read()
    if not status:
        raise Exception(f"Error reading video {video['filename']}")
    return frame

def main(yaml_config_file, output_folder):

    with open(yaml_config_file, "r") as stream:
        try:
            config = yaml.safe_load(stream)
        except:
            raise Exception(f"Error parsing configuration file '{yaml_config_file}'")

    if 'videos' not in config:
        raise NameError(f"Configuration file is missing paramater 'videos'")

    for nth, video in enumerate(config['videos']):
        if 'filename' not in video:
            raise NameError(f"Video #{nth} is missing 'filename' parameter")
        video['stream'] = cv2.VideoCapture(video['filename'])
        video['total_frames'] = int(video['stream'].get(cv2.CAP_PROP_FRAME_COUNT))
        if video['total_frames'] <= 0:
            raise Exception(f"Couldn't read video stream from file '{video['filename']}'")
        stemname = stem(video['filename'])
        video['output_path'] = path.join(output_folder, stemname)
        makedir(video['output_path'])
        if glob(path.join(video['output_path'], f"*.jpeg")):
            sys.stderr.write(f"[WARNING] Directory {video['output_path']} is not empty. Any existing files will be overwritten.\n")

    total_frames = min(_['total_frames'] for _ in config['videos'])
    sample_rate = max(1, total_frames // int(config.get('sample_n_frames', total_frames)))

    for nth_frame in tqdm(range(0, total_frames, sample_rate)):
        frames = []
        for video in config['videos']:
            frames.append(get_frame(video['stream'], nth_frame))

        for frame, video in zip(frames, config['videos']):
            frame = reorient(
                frame,
                rotate=video.get('rotate', 0),
                v_flip=video.get('vertical_flip', False),
                h_flip=video.get('horizontal_flip', False),
            )
            cv2.imwrite(path.join(video['output_path'], f"{nth_frame:06}.jpeg"), frame)

if __name__ == "__main__":
    debug = False
    try:
        if len(sys.argv) < 3:
            raise Exception("You have to provide a configuration file and a folder of where to save the frames (python3 extractFrames.py config.yml frames_folder)")
        debug = len(sys.argv) >= 4 and sys.argv[3] == 'debug'
        main(sys.argv[1], sys.argv[2])
    except Exception as e:
        if debug:
            traceback.print_exc()
        sys.stderr.write(f"[ERROR] {e}\n")
        sys.exit(1)
