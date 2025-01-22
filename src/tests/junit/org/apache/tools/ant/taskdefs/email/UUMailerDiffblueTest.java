package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class UUMailerDiffblueTest {
  /**
   * Test {@link UUMailer#attach(File, PrintStream)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code begin 644} toFile.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UUMailer#attach(File, PrintStream)}
   */
  @Test
  public void testAttach_whenPropertyIsJavaIoTmpdirIsBegin644ToFile_thenThrowBuildException() throws IOException {
    // Arrange
    UUMailer uuMailer = new UUMailer();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "begin 644 ").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> uuMailer.attach(file, new PrintStream(new ByteArrayOutputStream(1))));
  }

  /**
   * Test new {@link UUMailer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link UUMailer}
   */
  @Test
  public void testNewUUMailer() {
    // Arrange and Act
    UUMailer actualUuMailer = new UUMailer();

    // Assert
    assertFalse(actualUuMailer.isPortExplicitlySpecified());
    assertFalse(actualUuMailer.isStartTLSEnabled());
  }
}
