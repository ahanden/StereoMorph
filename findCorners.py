import sys
import cv2
from os import path, makedirs
from tqdm import tqdm
import yaml
from glob import glob
import traceback

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

def write_corners(file_path, corners):
    with open(file_path, "w") as stream:
        for row in corners:
            stream.write(f"{row[0][0]}\t{row[0][1]}\n")

def next_frame(video_stream):
    status, frame = video_stream.read()
    if not status:
        raise Exception(f"Error reading video {video['filename']}")
    return frame

def main(yaml_config_file):

    criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 30, 0.001)

    with open(yaml_config_file, "r") as stream:
        try:
            config = yaml.safe_load(stream)
        except:
            raise Exception(f"Error parsing configuration file '{yaml_config_file}'")

    for param in ('nx', 'ny', 'videos', 'corner_dir', 'verify_dir'):
       if param not in config:
            raise NameError(f"Configuration file is missing paramater '{param}'")

    for dim in ('nx', 'ny'):
        if int(config[dim]) < 2:
            raise ValueError(f"{dim} must be >= 2")

    chessboard_dims = (int(config['nx']), int(config['ny']))

    for dir_key in ('corner_dir', 'verify_dir'):
        makedir(config[dir_key])

    for nth, video in enumerate(config['videos']):
        if 'filename' not in video:
            raise NameError(f"Video #{nth} is missing 'filename' parameter")
        video['stream'] = cv2.VideoCapture(video['filename'])
        video['total_frames'] = int(video['stream'].get(cv2.CAP_PROP_FRAME_COUNT))
        if video['total_frames'] <= 0:
            raise Exception(f"Couldn't read video stream from file '{video['filename']}'")
        video['found_corners'] = 0
        stemname = stem(video['filename'])

        for dir_key, ext in (('corner_dir',  '.txt'), ('verify_dir', '.jpeg')):
            video[dir_key] = path.join(config[dir_key], stemname)
            makedir(video[dir_key])
            if glob(path.join(video[dir_key], f"*{ext}")):
                sys.stderr.write(f"[WARNING] Directory {video[dir_key]} is not empty. Any existing files will be overwritten.\n")

    total_frames = min(_['total_frames'] for _ in config['videos'])
    sample_rate = max(1, total_frames // int(config.get('sample_n_frames', total_frames)))

    for nth_frame in tqdm(range(total_frames)):
        frames = []
        for video in config['videos']:
            frames.append(next_frame(video['stream']))

        if nth_frame % sample_rate != 0:
            continue

        for frame, video in zip(frames, config['videos']):
            frame = reorient(
                frame,
                rotate=video.get('rotate', 0),
                v_flip=video.get('vertical_flip', False),
                h_flip=video.get('horizontal_flip', False),
            )
            gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

            corners_found, corners = cv2.findChessboardCorners(gray, chessboard_dims)

            if corners_found:
                video['found_corners'] += 1
                corners = cv2.cornerSubPix(gray, corners, (11,11), (-1,-1), criteria)
                write_corners(path.join(video['corner_dir'], f"{nth_frame:06}.txt"), corners)

            frame = cv2.drawChessboardCorners(frame, chessboard_dims, corners, corners_found)
            cv2.imwrite(path.join(video['verify_dir'], f"{nth_frame:06}.jpeg"), frame)

    print("Results:")
    for video in config['videos']:
        print(f"{path.basename(video['filename'])}: {video['found_corners']}")
        if len(glob(path.join(video['corner_dir'], "*.txt"))) > video['found_corners']:
            sys.stderr.write(f"[WARNING] Directory {video['corner_dir']} has more text files than there should be! This may screw things up downstream.\n")

if __name__ == "__main__":
    debug = False
    try:
        if len(sys.argv) < 2:
            raise Exception("You have to provide a configuration file (python3 findCorners.py config.yml)")
        if len(sys.argv) >= 3 and sys.argv[2] == 'debug':
            debug = True
        main(sys.argv[1])
    except Exception as e:
        if debug:
            traceback.print_exc()
        sys.stderr.write(f"[ERROR] {e}\n")
        sys.exit(1)
