package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import java.nio.file.Paths;
import org.junit.Test;

public class CompressedResourceDiffblueTest {
  /**
   * Test {@link CompressedResource#toString()}.
   * <ul>
   *   <li>Then return {@code Bzip2 compressed test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompressedResource#toString()}
   */
  @Test
  public void testToString_thenReturnBzip2CompressedTestTxt() {
    // Arrange, Act and Assert
    assertEquals("Bzip2 compressed test.txt",
        (new BZip2Resource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())))
            .toString());
  }
}
