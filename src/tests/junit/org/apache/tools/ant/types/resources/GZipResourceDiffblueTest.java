package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import org.junit.Test;

public class GZipResourceDiffblueTest {
  /**
   * Test {@link GZipResource#wrapStream(OutputStream)} with {@code out}.
   * <p>
   * Method under test: {@link GZipResource#wrapStream(OutputStream)}
   */
  @Test
  public void testWrapStreamWithOut() throws IOException {
    // Arrange
    GZipResource gZipResource = new GZipResource();
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    gZipResource.wrapStream(out);

    // Assert
    assertArrayEquals(new byte[]{31, -117, '\b', 0, 0, 0, 0, 0, 0, -1}, out.toByteArray());
  }

  /**
   * Test {@link GZipResource#getCompressionName()}.
   * <p>
   * Method under test: {@link GZipResource#getCompressionName()}
   */
  @Test
  public void testGetCompressionName() {
    // Arrange, Act and Assert
    assertEquals("GZip", (new GZipResource()).getCompressionName());
  }
}
