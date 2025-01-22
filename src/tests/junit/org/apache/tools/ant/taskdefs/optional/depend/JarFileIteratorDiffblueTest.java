package org.apache.tools.ant.taskdefs.optional.depend;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayInputStream;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JarFileIteratorDiffblueTest {
  /**
   * Test {@link JarFileIterator#JarFileIterator(InputStream)}.
   * <ul>
   *   <li>Then return NextClassFile is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarFileIterator#JarFileIterator(InputStream)}
   */
  @Test
  public void testNewJarFileIterator_thenReturnNextClassFileIsNull() throws IOException {
    // Arrange
    ByteArrayInputStream stream = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    JarFileIterator actualJarFileIterator = new JarFileIterator(stream);

    // Assert
    assertNull(actualJarFileIterator.getNextClassFile());
    assertEquals(-1, stream.read(new byte[]{}));
    assertFalse(actualJarFileIterator.iterator().hasNext());
  }

  /**
   * Test {@link JarFileIterator#getNextClassFile()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarFileIterator#getNextClassFile()}
   */
  @Test
  public void testGetNextClassFile_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new JarFileIterator(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).getNextClassFile());
  }

  /**
   * Test {@link JarFileIterator#getNextClassFile()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarFileIterator#getNextClassFile()}
   */
  @Test
  public void testGetNextClassFile_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> (new JarFileIterator(new FileInputStream(new FileDescriptor()))).getNextClassFile());
  }
}
