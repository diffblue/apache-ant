package org.apache.tools.ant.taskdefs.optional.native2ascii;

import static org.junit.Assert.assertThrows;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.optional.Native2Ascii;
import org.junit.Test;

public class BuiltinNative2AsciiDiffblueTest {
  /**
   * Test {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}.
   * <p>
   * Method under test: {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}
   */
  @Test
  public void testConvert() throws BuildException {
    // Arrange
    BuiltinNative2Ascii builtinNative2Ascii = new BuiltinNative2Ascii();

    Native2Ascii args = new Native2Ascii();
    args.setEncoding("UTF-8");
    File srcFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> builtinNative2Ascii.convert(args, srcFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "Exception trying to translate data", "foo").toFile()));
  }

  /**
   * Test {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}.
   * <ul>
   *   <li>Given {@code false}.</li>
   *   <li>When {@link Native2Ascii} (default constructor) Reverse is {@code false}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}
   */
  @Test
  public void testConvert_givenFalse_whenNative2AsciiReverseIsFalse_thenThrowBuildException() throws BuildException {
    // Arrange
    BuiltinNative2Ascii builtinNative2Ascii = new BuiltinNative2Ascii();

    Native2Ascii args = new Native2Ascii();
    args.setReverse(false);
    args.setEncoding("Args");
    File srcFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> builtinNative2Ascii.convert(args, srcFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Native2Ascii} (default constructor) Reverse is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}
   */
  @Test
  public void testConvert_givenTrue_whenNative2AsciiReverseIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    BuiltinNative2Ascii builtinNative2Ascii = new BuiltinNative2Ascii();

    Native2Ascii args = new Native2Ascii();
    args.setReverse(true);
    File srcFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> builtinNative2Ascii.convert(args, srcFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}.
   * <ul>
   *   <li>Given {@code UTF-8}.</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ASCII} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}
   */
  @Test
  public void testConvert_givenUtf8_whenPropertyIsJavaIoTmpdirIsAsciiToFile() throws BuildException {
    // Arrange
    BuiltinNative2Ascii builtinNative2Ascii = new BuiltinNative2Ascii();

    Native2Ascii args = new Native2Ascii();
    args.setEncoding("UTF-8");
    File srcFile = Paths.get(System.getProperty("java.io.tmpdir"), "ASCII").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> builtinNative2Ascii.convert(args, srcFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}.
   * <ul>
   *   <li>When {@link Native2Ascii} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BuiltinNative2Ascii#convert(Native2Ascii, File, File)}
   */
  @Test
  public void testConvert_whenNative2Ascii_thenThrowBuildException() throws BuildException {
    // Arrange
    BuiltinNative2Ascii builtinNative2Ascii = new BuiltinNative2Ascii();
    Native2Ascii args = new Native2Ascii();
    File srcFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> builtinNative2Ascii.convert(args, srcFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
