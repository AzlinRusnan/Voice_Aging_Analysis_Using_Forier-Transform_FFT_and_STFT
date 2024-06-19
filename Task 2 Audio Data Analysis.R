# Step 1: Load and Visualize Waveforms

library(tuneR)
library(seewave)
library(ggplot2)

# Load audio files
audio_2001 <- readWave("C:/Users/User/UKM - Nur Azlin Binti Rusnan/Sem 2/Unstructured Data/Project 3/Task_2/Audio/Siti Nurhaliza_Azimat Cinta_2001_cut.wav")
audio_2022 <- readWave("C:/Users/User/UKM - Nur Azlin Binti Rusnan/Sem 2/Unstructured Data/Project 3/Task_2/Audio/Siti Nurhaliza_Azimat Cinta_2022_cut.wav")

# Normalize audio data
audio_2001 <- normalize(audio_2001, unit = "32")
audio_2022 <- normalize(audio_2022, unit = "32")
play(audio_2001)
play(audio_2022)

# Step 1: Wave Signal Analysis
# We will plot the waveforms of both audio files to visualize the wave signals.

plot_wave_signal <- function(audio, title) {
  audio_data <- audio@left / 32768
  time <- seq(0, length(audio_data) - 1) / audio@samp.rate
  plot(time, audio_data, type = "l", xlab = "Time (s)", ylab = "Amplitude", main = title)
}

# Plot Wave Signals
par(mfrow = c(2, 1))
plot_wave_signal(audio_2001, "Waveform of Siti Nurhaliza - Azimat Cinta (2001)")
plot_wave_signal(audio_2022, "Waveform of Siti Nurhaliza - Azimat Cinta (2022)")

# Step 2: Fourier Transform (FFT) Analysis
# We will perform FFT to analyze the frequency components of the audio signals.

plot_frequency_spectrum <- function(audio, title) {
  audio_data <- audio@left / 32768
  n <- length(audio_data)
  freq <- (0:(n - 1)) * (audio@samp.rate / n)
  fft_data <- abs(fft(audio_data))
  plot(freq[1:(n / 2)], fft_data[1:(n / 2)], type = "l", xlab = "Frequency (Hz)", ylab = "Amplitude", main = title)
}

# Plot Frequency Spectrum
par(mfrow = c(2, 1))
plot_frequency_spectrum(audio_2001, "Frequency Spectrum of Siti Nurhaliza - Azimat Cinta (2001)")
plot_frequency_spectrum(audio_2022, "Frequency Spectrum of Siti Nurhaliza - Azimat Cinta (2022)")

# Plot a zoomed-in frequency spectrum
par(mfrow = c(2, 1))
plot_frequency_spectrum(audio_2001[1:20000], "Frequency Spectrum of Siti Nurhaliza - Azimat Cinta (2022) - Zoomed")
plot_frequency_spectrum(audio_2022[1:20000], "Frequency Spectrum of Siti Nurhaliza - Azimat Cinta (2022) - Zoomed")

# Step 3: Short-Time Fourier Transform (STFT) Analysis
# We will perform STFT to analyze how the frequency content of the audio signals changes over time.

plot_spectrogram <- function(audio, title) {
  spectro(audio, wl = 1024, ovlp = 50, osc = TRUE, main = title)
}

# Plot Spectrogram
par(mfrow = c(2, 1))
plot_spectrogram(audio_2001, "Spectrogram (2001)")
plot_spectrogram(audio_2022, "Spectrogram (2022)")
