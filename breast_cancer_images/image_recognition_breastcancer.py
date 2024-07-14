import numpy as np
import pandas as pd
from fastai.vision.all import *
import matplotlib.image as mpimg
import matplotlib.pyplot as plt
from pyhere import here

path = untar_data("c:/Users/cpppe/Desktop/github_projects/data_visualizations/breat_cancer_images/Dataset_BUSI_with_GT")

# Read Images
img_nor = mpimg.imread("Dataset_BUSI_with_GT/normal/normal (1)")
img_mag = mpimg.imread("Dataset_BUSI_with_GT/malignant/malignant (1)")
 
# Output Images
plt.imshow(img)

#Path.cwd()

train_img = get_image_files("c:/Users/cpppe/Desktop/github_projects/data_visualizations/breat_cancer_images/Dataset_BUSI_with_GT")

#train_img = list(train_normal_img) + list(train_case_img)

len(train_img)

#train_img[184]
#train_img[5215]

#train_series = pd.Series(train_img, dtype = "string") 
#train_series.info()


#train_series.str.contains('person')

person_image_files = [p for p in train_img if 'person' in p.name]

def label_func(fname):
    return re.search(r'person', fname.name) is not None


dls = ImageDataLoaders.from_path_func(
    "c:/Users/cpppe/Desktop/github_projects/data_visualizations/pneumonia_images/chest_xray/train",
    person_image_files,
    label_func,
    valid_pct=0.2,
    seed = 42,
    item_tfms=Resize(224)
    )

learn = vision_learner(dls, resnet34, metrics=error_rate)
learn.fine_tune(1)

learn.predict(files[0:9])
learn.show_results()

