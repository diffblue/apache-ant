package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.junit.Test;

public class ConcatFileInputStreamDiffblueTest {
  /**
   * Test {@link ConcatFileInputStream#read()}.
   * <ul>
   *   <li>Given {@link ConcatFileInputStream#ConcatFileInputStream(File[])} with file is empty array of {@link File}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatFileInputStream#read()}
   */
  @Test
  public void testRead_givenConcatFileInputStreamWithFileIsEmptyArrayOfFile() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatFileInputStream(new File[]{})).read());
  }

  /**
   * Test {@link ConcatFileInputStream#read()}.
   * <ul>
   *   <li>Given {@link ConcatFileInputStream#ConcatFileInputStream(File[])} with file is {@code null}.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatFileInputStream#read()}
   */
  @Test
  public void testRead_givenConcatFileInputStreamWithFileIsNull_thenReturnRetry_forever() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatFileInputStream(null)).read());
  }
}
