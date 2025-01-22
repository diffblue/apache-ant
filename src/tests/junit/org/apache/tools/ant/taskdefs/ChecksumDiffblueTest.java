package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.text.Format;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Set;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Checksum.FormatElement;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class ChecksumDiffblueTest {
  /**
   * Test {@link Checksum#execute()}.
   * <p>
   * Method under test: {@link Checksum#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    checksum.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.execute());
  }

  /**
   * Test {@link Checksum#execute()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#execute()}
   */
  @Test
  public void testExecute_givenChecksumAddSystemBootClasspath_thenThrowBuildException() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.execute());
  }

  /**
   * Test {@link Checksum#execute()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#execute()}
   */
  @Test
  public void testExecute_givenChecksumAddSystemBootClasspath_thenThrowBuildException2() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setFile(Copy.NULL_FILE_PLACEHOLDER);
    checksum.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.execute());
  }

  /**
   * Test {@link Checksum#execute()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#execute()}
   */
  @Test
  public void testExecute_givenChecksumFileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.execute());
  }

  /**
   * Test {@link Checksum#execute()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#execute()}
   */
  @Test
  public void testExecute_givenChecksum_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Checksum()).execute());
  }

  /**
   * Test {@link Checksum#eval()}.
   * <p>
   * Method under test: {@link Checksum#eval()}
   */
  @Test
  public void testEval() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    checksum.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.eval());
  }

  /**
   * Test {@link Checksum#eval()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#eval()}
   */
  @Test
  public void testEval_givenChecksumAddSystemBootClasspath_thenThrowBuildException() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.eval());
  }

  /**
   * Test {@link Checksum#eval()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) add {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#eval()}
   */
  @Test
  public void testEval_givenChecksumAddSystemBootClasspath_thenThrowBuildException2() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setFile(Copy.NULL_FILE_PLACEHOLDER);
    checksum.add(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.eval());
  }

  /**
   * Test {@link Checksum#eval()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#eval()}
   */
  @Test
  public void testEval_givenChecksumFileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Checksum checksum = new Checksum();
    checksum.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> checksum.eval());
  }

  /**
   * Test {@link Checksum#eval()}.
   * <ul>
   *   <li>Given {@link Checksum} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Checksum#eval()}
   */
  @Test
  public void testEval_givenChecksum_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Checksum()).eval());
  }

  /**
   * Test {@link Checksum#decodeHex(char[])}.
   * <p>
   * Method under test: {@link Checksum#decodeHex(char[])}
   */
  @Test
  public void testDecodeHex() throws BuildException {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{-1, -1}, Checksum.decodeHex("AZAZ".toCharArray()));
  }

  /**
   * Test FormatElement {@link FormatElement#getDefault()}.
   * <p>
   * Method under test: {@link FormatElement#getDefault()}
   */
  @Test
  public void testFormatElementGetDefault() {
    // Arrange and Act
    FormatElement actualDefault = FormatElement.getDefault();

    // Assert
    assertEquals("CHECKSUM", actualDefault.getValue());
    assertEquals(0, actualDefault.getIndex());
    MessageFormat format = actualDefault.getFormat();
    assertEquals(1, format.getFormats().length);
    assertEquals(1, format.getFormatsByArgumentIndex().length);
    assertArrayEquals(new String[]{"CHECKSUM", "MD5SUM", "SVF"}, actualDefault.getValues());
  }

  /**
   * Test FormatElement {@link FormatElement#getFormat()}.
   * <p>
   * Method under test: {@link FormatElement#getFormat()}
   */
  @Test
  public void testFormatElementGetFormat() throws MissingResourceException {
    // Arrange and Act
    MessageFormat actualFormat = FormatElement.getDefault().getFormat();

    // Assert
    Locale locale = actualFormat.getLocale();
    assertEquals("", locale.getDisplayScript());
    assertEquals("", locale.getDisplayVariant());
    assertEquals("", locale.getScript());
    assertEquals("", locale.getVariant());
    assertEquals("English (United Kingdom)", locale.getDisplayName());
    assertEquals("English", locale.getDisplayLanguage());
    assertEquals("GB", locale.getCountry());
    assertEquals("GBR", locale.getISO3Country());
    assertEquals("United Kingdom", locale.getDisplayCountry());
    assertEquals("en", locale.getLanguage());
    assertEquals("eng", locale.getISO3Language());
    Format[] formats = actualFormat.getFormats();
    assertNull(formats[0]);
    Format[] formatsByArgumentIndex = actualFormat.getFormatsByArgumentIndex();
    assertNull(formatsByArgumentIndex[0]);
    assertEquals(1, formats.length);
    assertEquals(1, formatsByArgumentIndex.length);
    assertFalse(locale.hasExtensions());
    Set<Character> extensionKeys = locale.getExtensionKeys();
    assertTrue(extensionKeys.isEmpty());
    assertSame(extensionKeys, locale.getUnicodeLocaleAttributes());
    assertSame(extensionKeys, locale.getUnicodeLocaleKeys());
  }

  /**
   * Test FormatElement {@link FormatElement#getValues()}.
   * <p>
   * Method under test: {@link FormatElement#getValues()}
   */
  @Test
  public void testFormatElementGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"CHECKSUM", "MD5SUM", "SVF"}, FormatElement.getDefault().getValues());
  }

  /**
   * Test FormatElement new {@link FormatElement} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FormatElement}
   */
  @Test
  public void testFormatElementNewFormatElement() {
    // Arrange and Act
    FormatElement actualFormatElement = new FormatElement();

    // Assert
    assertNull(actualFormatElement.getValue());
    assertEquals(-1, actualFormatElement.getIndex());
  }

  /**
   * Test new {@link Checksum} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Checksum}
   */
  @Test
  public void testNewChecksum() {
    // Arrange and Act
    Checksum actualChecksum = new Checksum();

    // Assert
    assertNull(actualChecksum.getDescription());
    assertNull(actualChecksum.getTaskName());
    assertNull(actualChecksum.getTaskType());
    assertNull(actualChecksum.getProject());
    assertNull(actualChecksum.getOwningTarget());
    assertFalse(actualChecksum.hasSelectors());
  }
}
