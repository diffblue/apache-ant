package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import org.junit.Test;

public class DemuxInputStreamDiffblueTest {
  /**
   * Test {@link DemuxInputStream#read(byte[], int, int)} with {@code byte[]}, {@code int}, {@code int}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link DemuxInputStream#read(byte[], int, int)}
   */
  @Test
  public void testReadWithByteIntInt_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    DemuxInputStream demuxInputStream = new DemuxInputStream(project);

    // Act and Assert
    assertEquals(3, demuxInputStream.read("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link DemuxInputStream#read()}.
   * <ul>
   *   <li>Given {@code A}.</li>
   *   <li>Then return sixty-five.</li>
   * </ul>
   * <p>
   * Method under test: {@link DemuxInputStream#read()}
   */
  @Test
  public void testRead_givenA_thenReturnSixtyFive() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act and Assert
    assertEquals(65, (new DemuxInputStream(project)).read());
  }

  /**
   * Test {@link DemuxInputStream#read()}.
   * <ul>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DemuxInputStream#read()}
   */
  @Test
  public void testRead_thenReturnMinusOne() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream(new byte[]{}));

    // Act and Assert
    assertEquals(-1, (new DemuxInputStream(project)).read());
  }
}
