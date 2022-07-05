package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import org.junit.Test;

public class OutputStreamFunnelerDiffblueTest {
  /**
  * Method under test: {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream)}
  */
  @Test
  public void testConstructor() {
    // Arrange
    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(1);

    // Act
    new OutputStreamFunneler(byteArrayOutputStream);

    // Assert
    assertEquals(0, byteArrayOutputStream.size());
  }

  /**
   * Method under test: {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream)}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new OutputStreamFunneler(null));
  }

  /**
   * Method under test: {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream, long)}
   */
  @Test
  public void testConstructor3() {
    // Arrange
    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(1);

    // Act
    new OutputStreamFunneler(byteArrayOutputStream, 10L);

    // Assert
    assertEquals(0, byteArrayOutputStream.size());
  }

  /**
   * Method under test: {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream, long)}
   */
  @Test
  public void testConstructor4() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> new OutputStreamFunneler(null, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));

  }
}

