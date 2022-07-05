package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.ant.antunit.AntUnit;
import org.apache.tools.ant.ProjectComponent;
import org.junit.Test;

public class ConcatFileInputStreamDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link ConcatFileInputStream#ConcatFileInputStream(File[])}
  *   <li>{@link ConcatFileInputStream#setManagingComponent(ProjectComponent)}
  * </ul>
  */
  @Test
  public void testConstructor() throws IOException {
    // Arrange and Act
    ConcatFileInputStream actualConcatFileInputStream = new ConcatFileInputStream(
        new File[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()});
    actualConcatFileInputStream.setManagingComponent(new AntUnit());

    // Assert
    assertEquals(0, actualConcatFileInputStream.available());
  }

  /**
   * Method under test: {@link ConcatFileInputStream#read()}
   */
  @Test
  public void testRead() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatFileInputStream(null)).read());
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatFileInputStream(new File[]{})).read());
  }
}

