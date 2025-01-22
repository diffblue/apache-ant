package org.apache.tools.tar;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class TarEntryDiffblueTest {
  /**
   * Test {@link TarEntry#TarEntry(File, String)}.
   * <ul>
   *   <li>Then return Name is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(File, String)}
   */
  @Test
  public void testNewTarEntry_thenReturnNameIsFooTxt() {
    // Arrange
    File file = Paths.get(System.getProperty("java.io.tmpdir"), TarConstants.VERSION_POSIX).toFile();

    // Act
    TarEntry actualTarEntry = new TarEntry(file, "foo.txt");

    // Assert
    assertEquals("foo.txt", actualTarEntry.getName());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('0', actualTarEntry.getLinkFlag());
    assertSame(file, actualTarEntry.getFile());
  }

  /**
   * Test {@link TarEntry#TarEntry(File, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(File, String)}
   */
  @Test
  public void testNewTarEntry_whenEmptyString_thenReturnNameIsSlash() {
    // Arrange
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    TarEntry actualTarEntry = new TarEntry(file, "");

    // Assert
    assertEquals("/", actualTarEntry.getName());
    assertEquals(TarEntry.DEFAULT_DIR_MODE, actualTarEntry.getMode());
    assertEquals('5', actualTarEntry.getLinkFlag());
    assertSame(file, actualTarEntry.getFile());
  }

  /**
   * Test {@link TarEntry#TarEntry(File, String)}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return Name is {@code foo.txt/}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(File, String)}
   */
  @Test
  public void testNewTarEntry_whenFooTxt_thenReturnNameIsFooTxt() {
    // Arrange
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    TarEntry actualTarEntry = new TarEntry(file, "foo.txt");

    // Assert
    assertEquals("foo.txt/", actualTarEntry.getName());
    assertEquals(TarEntry.DEFAULT_DIR_MODE, actualTarEntry.getMode());
    assertEquals('5', actualTarEntry.getLinkFlag());
    assertSame(file, actualTarEntry.getFile());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, byte)}.
   * <ul>
   *   <li>When {@code L}.</li>
   *   <li>Then return GNULongNameEntry.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, byte)}
   */
  @Test
  public void testNewTarEntry_whenL_thenReturnGNULongNameEntry() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("Name", (byte) 'L');

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("Name", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertTrue(actualTarEntry.isGNULongNameEntry());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('L', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String)}
   */
  @Test
  public void testNewTarEntry_whenName_thenReturnName() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("Name");

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("Name", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('0', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, byte)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, byte)}
   */
  @Test
  public void testNewTarEntry_whenName_thenReturnName2() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("Name", (byte) 'A');

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("Name", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('A', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, boolean)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, boolean)}
   */
  @Test
  public void testNewTarEntry_whenName_thenReturnName3() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("Name", true);

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("Name", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('0', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, boolean)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, boolean)}
   */
  @Test
  public void testNewTarEntry_whenName_thenReturnName4() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("Name", false);

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("Name", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('0', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String)}
   */
  @Test
  public void testNewTarEntry_whenSlash_thenReturnNameIsEmptyString() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("/");

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getName());
    assertEquals("", actualTarEntry.getUserName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('0', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, byte)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, byte)}
   */
  @Test
  public void testNewTarEntry_whenSlash_thenReturnNameIsEmptyString2() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("/", (byte) 'A');

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getName());
    assertEquals("", actualTarEntry.getUserName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('A', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, boolean)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, boolean)}
   */
  @Test
  public void testNewTarEntry_whenSlash_thenReturnNameIsEmptyString3() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("/", false);

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getName());
    assertEquals("", actualTarEntry.getUserName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_FILE_MODE, actualTarEntry.getMode());
    assertEquals('0', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(File, String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(File, String)}
   */
  @Test
  public void testNewTarEntry_whenSlash_thenReturnNameIsSlash() {
    // Arrange
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    TarEntry actualTarEntry = new TarEntry(file, "/");

    // Assert
    assertEquals("/", actualTarEntry.getName());
    assertEquals(TarEntry.DEFAULT_DIR_MODE, actualTarEntry.getMode());
    assertEquals('5', actualTarEntry.getLinkFlag());
    assertSame(file, actualTarEntry.getFile());
  }

  /**
   * Test {@link TarEntry#TarEntry(String, boolean)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String, boolean)}
   */
  @Test
  public void testNewTarEntry_whenSlash_thenReturnNameIsSlash2() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("/", true);

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("/", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_DIR_MODE, actualTarEntry.getMode());
    assertEquals('5', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#TarEntry(String)}.
   * <ul>
   *   <li>When ustar null /.</li>
   *   <li>Then return Name is ustar null /.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#TarEntry(String)}
   */
  @Test
  public void testNewTarEntry_whenUstarNull_thenReturnNameIsUstarNull() {
    // Arrange and Act
    TarEntry actualTarEntry = new TarEntry("ustar\u0000/");

    // Assert
    assertEquals("", actualTarEntry.getGroupName());
    assertEquals("", actualTarEntry.getLinkName());
    assertEquals("", actualTarEntry.getUserName());
    assertEquals("ustar\u0000/", actualTarEntry.getName());
    assertNull(actualTarEntry.getFile());
    assertEquals(0, actualTarEntry.getDevMajor());
    assertEquals(0, actualTarEntry.getDevMinor());
    assertEquals(0, actualTarEntry.getGroupId());
    assertEquals(0, actualTarEntry.getUserId());
    assertEquals(0, actualTarEntry.getDirectoryEntries().length);
    assertEquals(0L, actualTarEntry.getLongGroupId());
    assertEquals(0L, actualTarEntry.getLongUserId());
    assertEquals(0L, actualTarEntry.getRealSize());
    assertEquals(0L, actualTarEntry.getSize());
    assertFalse(actualTarEntry.isBlockDevice());
    assertFalse(actualTarEntry.isCharacterDevice());
    assertFalse(actualTarEntry.isExtended());
    assertFalse(actualTarEntry.isFIFO());
    assertFalse(actualTarEntry.isGNULongLinkEntry());
    assertFalse(actualTarEntry.isGNULongNameEntry());
    assertFalse(actualTarEntry.isGNUSparse());
    assertFalse(actualTarEntry.isGlobalPaxHeader());
    assertFalse(actualTarEntry.isLink());
    assertFalse(actualTarEntry.isPaxHeader());
    assertFalse(actualTarEntry.isSymbolicLink());
    assertEquals(TarEntry.DEFAULT_DIR_MODE, actualTarEntry.getMode());
    assertEquals('5', actualTarEntry.getLinkFlag());
  }

  /**
   * Test {@link TarEntry#equals(Object)}, and {@link TarEntry#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarEntry#equals(Object)}
   *   <li>{@link TarEntry#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    TarEntry tarEntry2 = new TarEntry("Name");

    // Act and Assert
    assertEquals(tarEntry, tarEntry2);
    int expectedHashCodeResult = tarEntry.hashCode();
    assertEquals(expectedHashCodeResult, tarEntry2.hashCode());
  }

  /**
   * Test {@link TarEntry#equals(Object)}, and {@link TarEntry#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarEntry#equals(Object)}
   *   <li>{@link TarEntry#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act and Assert
    assertEquals(tarEntry, tarEntry);
    int expectedHashCodeResult = tarEntry.hashCode();
    assertEquals(expectedHashCodeResult, tarEntry.hashCode());
  }

  /**
   * Test {@link TarEntry#equals(TarEntry)} with {@code TarEntry}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#equals(TarEntry)}
   */
  @Test
  public void testEqualsWithTarEntry_givenTarEntryWithNameIsEmptyString_thenReturnFalse() {
    // Arrange
    TarEntry tarEntry = new TarEntry("");

    // Act and Assert
    assertFalse(tarEntry.equals(new TarEntry("Name")));
  }

  /**
   * Test {@link TarEntry#equals(TarEntry)} with {@code TarEntry}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#equals(TarEntry)}
   */
  @Test
  public void testEqualsWithTarEntry_givenTarEntryWithName_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).equals((TarEntry) null));
  }

  /**
   * Test {@link TarEntry#equals(TarEntry)} with {@code TarEntry}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>When {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#equals(TarEntry)}
   */
  @Test
  public void testEqualsWithTarEntry_givenTarEntryWithName_whenTarEntryWithName_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act and Assert
    assertTrue(tarEntry.equals(new TarEntry("Name")));
  }

  /**
   * Test {@link TarEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    TarEntry tarEntry = new TarEntry("");

    // Act and Assert
    assertNotEquals(tarEntry, new TarEntry("Name"));
  }

  /**
   * Test {@link TarEntry#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new TarEntry("Name"), null);
  }

  /**
   * Test {@link TarEntry#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new TarEntry("Name"), "Different type to TarEntry");
  }

  /**
   * Test {@link TarEntry#isDescendent(TarEntry)}.
   * <ul>
   *   <li>Given {@code The characteristics of someone or something}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isDescendent(TarEntry)}
   */
  @Test
  public void testIsDescendent_givenTheCharacteristicsOfSomeoneOrSomething_thenReturnFalse() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    TarEntry desc = new TarEntry("Name");
    desc.setName("The characteristics of someone or something");

    // Act and Assert
    assertFalse(tarEntry.isDescendent(desc));
  }

  /**
   * Test {@link TarEntry#isDescendent(TarEntry)}.
   * <ul>
   *   <li>When {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isDescendent(TarEntry)}
   */
  @Test
  public void testIsDescendent_whenTarEntryWithName_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act and Assert
    assertTrue(tarEntry.isDescendent(new TarEntry("Name")));
  }

  /**
   * Test {@link TarEntry#setName(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then {@link TarEntry#TarEntry(String)} with {@code Name} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setName(String)}
   */
  @Test
  public void testSetName_whenName_thenTarEntryWithNameNameIsName() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setName("Name");

    // Assert that nothing has changed
    assertEquals("Name", tarEntry.getName());
  }

  /**
   * Test {@link TarEntry#setName(String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then {@link TarEntry#TarEntry(String)} with {@code Name} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setName(String)}
   */
  @Test
  public void testSetName_whenSlash_thenTarEntryWithNameNameIsEmptyString() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setName("/");

    // Assert
    assertEquals("", tarEntry.getName());
  }

  /**
   * Test {@link TarEntry#getUserId()}.
   * <p>
   * Method under test: {@link TarEntry#getUserId()}
   */
  @Test
  public void testGetUserId() {
    // Arrange, Act and Assert
    assertEquals(0, (new TarEntry("Name")).getUserId());
  }

  /**
   * Test {@link TarEntry#setUserId(int)} with {@code int}.
   * <p>
   * Method under test: {@link TarEntry#setUserId(int)}
   */
  @Test
  public void testSetUserIdWithInt() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setUserId(1);

    // Assert
    assertEquals(1, tarEntry.getUserId());
    assertEquals(1L, tarEntry.getLongUserId());
  }

  /**
   * Test {@link TarEntry#getGroupId()}.
   * <p>
   * Method under test: {@link TarEntry#getGroupId()}
   */
  @Test
  public void testGetGroupId() {
    // Arrange, Act and Assert
    assertEquals(0, (new TarEntry("Name")).getGroupId());
  }

  /**
   * Test {@link TarEntry#setGroupId(int)} with {@code int}.
   * <p>
   * Method under test: {@link TarEntry#setGroupId(int)}
   */
  @Test
  public void testSetGroupIdWithInt() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setGroupId(1);

    // Assert
    assertEquals(1, tarEntry.getGroupId());
    assertEquals(1L, tarEntry.getLongGroupId());
  }

  /**
   * Test {@link TarEntry#setIds(int, int)}.
   * <p>
   * Method under test: {@link TarEntry#setIds(int, int)}
   */
  @Test
  public void testSetIds() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setIds(1, 1);

    // Assert
    assertEquals(1, tarEntry.getGroupId());
    assertEquals(1, tarEntry.getUserId());
    assertEquals(1L, tarEntry.getLongGroupId());
    assertEquals(1L, tarEntry.getLongUserId());
  }

  /**
   * Test {@link TarEntry#setNames(String, String)}.
   * <p>
   * Method under test: {@link TarEntry#setNames(String, String)}
   */
  @Test
  public void testSetNames() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setNames("janedoe", "Group Name");

    // Assert
    assertEquals("Group Name", tarEntry.getGroupName());
    assertEquals("janedoe", tarEntry.getUserName());
  }

  /**
   * Test {@link TarEntry#setSize(long)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setSize(long)}
   */
  @Test
  public void testSetSize_whenMinusOne_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new TarEntry("Name")).setSize(-1L));
  }

  /**
   * Test {@link TarEntry#setSize(long)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then {@link TarEntry#TarEntry(String)} with {@code Name} Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setSize(long)}
   */
  @Test
  public void testSetSize_whenThree_thenTarEntryWithNameSizeIsThree() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setSize(3L);

    // Assert
    assertEquals(3L, tarEntry.getSize());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarEntry#setGroupId(long)}
   *   <li>{@link TarEntry#setGroupName(String)}
   *   <li>{@link TarEntry#setLinkFlag(byte)}
   *   <li>{@link TarEntry#setLinkName(String)}
   *   <li>{@link TarEntry#setModTime(long)}
   *   <li>{@link TarEntry#setMode(int)}
   *   <li>{@link TarEntry#setUserId(long)}
   *   <li>{@link TarEntry#setUserName(String)}
   *   <li>{@link TarEntry#getDevMajor()}
   *   <li>{@link TarEntry#getDevMinor()}
   *   <li>{@link TarEntry#getFile()}
   *   <li>{@link TarEntry#getGroupName()}
   *   <li>{@link TarEntry#getLinkFlag()}
   *   <li>{@link TarEntry#getLinkName()}
   *   <li>{@link TarEntry#getLongGroupId()}
   *   <li>{@link TarEntry#getLongUserId()}
   *   <li>{@link TarEntry#getMode()}
   *   <li>{@link TarEntry#getName()}
   *   <li>{@link TarEntry#getRealSize()}
   *   <li>{@link TarEntry#getSize()}
   *   <li>{@link TarEntry#getUserName()}
   *   <li>{@link TarEntry#isExtended()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setGroupId(1L);
    tarEntry.setGroupName("Group Name");
    tarEntry.setLinkFlag((byte) 'A');
    tarEntry.setLinkName("Link");
    tarEntry.setModTime(10L);
    tarEntry.setMode(1);
    tarEntry.setUserId(1L);
    tarEntry.setUserName("janedoe");
    int actualDevMajor = tarEntry.getDevMajor();
    int actualDevMinor = tarEntry.getDevMinor();
    File actualFile = tarEntry.getFile();
    String actualGroupName = tarEntry.getGroupName();
    byte actualLinkFlag = tarEntry.getLinkFlag();
    String actualLinkName = tarEntry.getLinkName();
    long actualLongGroupId = tarEntry.getLongGroupId();
    long actualLongUserId = tarEntry.getLongUserId();
    int actualMode = tarEntry.getMode();
    String actualName = tarEntry.getName();
    long actualRealSize = tarEntry.getRealSize();
    long actualSize = tarEntry.getSize();
    String actualUserName = tarEntry.getUserName();

    // Assert
    assertEquals("Group Name", actualGroupName);
    assertEquals("Link", actualLinkName);
    assertEquals("Name", actualName);
    assertEquals("janedoe", actualUserName);
    assertNull(actualFile);
    assertEquals(0, actualDevMajor);
    assertEquals(0, actualDevMinor);
    assertEquals(0L, actualRealSize);
    assertEquals(0L, actualSize);
    assertEquals(1, actualMode);
    assertEquals(1L, actualLongGroupId);
    assertEquals(1L, actualLongUserId);
    assertFalse(tarEntry.isExtended());
    assertEquals('A', actualLinkFlag);
  }

  /**
   * Test {@link TarEntry#setDevMajor(int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setDevMajor(int)}
   */
  @Test
  public void testSetDevMajor_whenMinusOne_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new TarEntry("Name")).setDevMajor(-1));
  }

  /**
   * Test {@link TarEntry#setDevMajor(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link TarEntry#TarEntry(String)} with {@code Name} DevMajor is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setDevMajor(int)}
   */
  @Test
  public void testSetDevMajor_whenOne_thenTarEntryWithNameDevMajorIsOne() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setDevMajor(1);

    // Assert
    assertEquals(1, tarEntry.getDevMajor());
  }

  /**
   * Test {@link TarEntry#setDevMinor(int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setDevMinor(int)}
   */
  @Test
  public void testSetDevMinor_whenMinusOne_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new TarEntry("Name")).setDevMinor(-1));
  }

  /**
   * Test {@link TarEntry#setDevMinor(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link TarEntry#TarEntry(String)} with {@code Name} DevMinor is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#setDevMinor(int)}
   */
  @Test
  public void testSetDevMinor_whenOne_thenTarEntryWithNameDevMinorIsOne() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");

    // Act
    tarEntry.setDevMinor(1);

    // Assert
    assertEquals(1, tarEntry.getDevMinor());
  }

  /**
   * Test {@link TarEntry#isGNUSparse()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code S}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGNUSparse()}
   */
  @Test
  public void testIsGNUSparse_givenTarEntryWithNameLinkFlagIsS_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) 'S');

    // Act and Assert
    assertTrue(tarEntry.isGNUSparse());
  }

  /**
   * Test {@link TarEntry#isGNUSparse()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGNUSparse()}
   */
  @Test
  public void testIsGNUSparse_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isGNUSparse());
  }

  /**
   * Test {@link TarEntry#isGNULongLinkEntry()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code K}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGNULongLinkEntry()}
   */
  @Test
  public void testIsGNULongLinkEntry_givenTarEntryWithNameLinkFlagIsK_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) 'K');

    // Act and Assert
    assertTrue(tarEntry.isGNULongLinkEntry());
  }

  /**
   * Test {@link TarEntry#isGNULongLinkEntry()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGNULongLinkEntry()}
   */
  @Test
  public void testIsGNULongLinkEntry_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isGNULongLinkEntry());
  }

  /**
   * Test {@link TarEntry#isGNULongNameEntry()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code L}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGNULongNameEntry()}
   */
  @Test
  public void testIsGNULongNameEntry_givenTarEntryWithNameLinkFlagIsL_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) 'L');

    // Act and Assert
    assertTrue(tarEntry.isGNULongNameEntry());
  }

  /**
   * Test {@link TarEntry#isGNULongNameEntry()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGNULongNameEntry()}
   */
  @Test
  public void testIsGNULongNameEntry_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isGNULongNameEntry());
  }

  /**
   * Test {@link TarEntry#isPaxHeader()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code x}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isPaxHeader()}
   */
  @Test
  public void testIsPaxHeader_givenTarEntryWithNameLinkFlagIsX_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) 'x');

    // Act and Assert
    assertTrue(tarEntry.isPaxHeader());
  }

  /**
   * Test {@link TarEntry#isPaxHeader()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code X}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isPaxHeader()}
   */
  @Test
  public void testIsPaxHeader_givenTarEntryWithNameLinkFlagIsX_thenReturnTrue2() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) 'X');

    // Act and Assert
    assertTrue(tarEntry.isPaxHeader());
  }

  /**
   * Test {@link TarEntry#isPaxHeader()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isPaxHeader()}
   */
  @Test
  public void testIsPaxHeader_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isPaxHeader());
  }

  /**
   * Test {@link TarEntry#isGlobalPaxHeader()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code g}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGlobalPaxHeader()}
   */
  @Test
  public void testIsGlobalPaxHeader_givenTarEntryWithNameLinkFlagIsG_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) 'g');

    // Act and Assert
    assertTrue(tarEntry.isGlobalPaxHeader());
  }

  /**
   * Test {@link TarEntry#isGlobalPaxHeader()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isGlobalPaxHeader()}
   */
  @Test
  public void testIsGlobalPaxHeader_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isGlobalPaxHeader());
  }

  /**
   * Test {@link TarEntry#isDirectory()}.
   * <p>
   * Method under test: {@link TarEntry#isDirectory()}
   */
  @Test
  public void testIsDirectory() {
    // Arrange
    TarEntry tarEntry = new TarEntry("/", true);
    tarEntry.setLinkFlag((byte) 'A');

    // Act and Assert
    assertTrue(tarEntry.isDirectory());
  }

  /**
   * Test {@link TarEntry#isDirectory()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String, boolean)} with name is {@code /} and preserveLeadingSlashes is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenTarEntryWithNameIsSlashAndPreserveLeadingSlashesIsTrue() {
    // Arrange, Act and Assert
    assertTrue((new TarEntry("/", true)).isDirectory());
  }

  /**
   * Test {@link TarEntry#isDirectory()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isDirectory());
  }

  /**
   * Test {@link TarEntry#isFile()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String, byte)} with {@code Name} and linkFlag is {@code A}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isFile()}
   */
  @Test
  public void testIsFile_givenTarEntryWithNameAndLinkFlagIsA_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new TarEntry("Name", (byte) 'A')).isFile());
  }

  /**
   * Test {@link TarEntry#isFile()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String, byte)} with {@code Name} and linkFlag is {@link TarConstants#LF_OLDNORM}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isFile()}
   */
  @Test
  public void testIsFile_givenTarEntryWithNameAndLinkFlagIsLf_oldnorm_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new TarEntry("Name", TarConstants.LF_OLDNORM)).isFile());
  }

  /**
   * Test {@link TarEntry#isFile()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name/}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isFile()}
   */
  @Test
  public void testIsFile_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name/")).isFile());
  }

  /**
   * Test {@link TarEntry#isFile()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isFile()}
   */
  @Test
  public void testIsFile_givenTarEntryWithName_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new TarEntry("Name")).isFile());
  }

  /**
   * Test {@link TarEntry#isSymbolicLink()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code 2}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isSymbolicLink()}
   */
  @Test
  public void testIsSymbolicLink_givenTarEntryWithNameLinkFlagIs2_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) '2');

    // Act and Assert
    assertTrue(tarEntry.isSymbolicLink());
  }

  /**
   * Test {@link TarEntry#isSymbolicLink()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isSymbolicLink()}
   */
  @Test
  public void testIsSymbolicLink_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isSymbolicLink());
  }

  /**
   * Test {@link TarEntry#isLink()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code 1}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isLink()}
   */
  @Test
  public void testIsLink_givenTarEntryWithNameLinkFlagIs1_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) '1');

    // Act and Assert
    assertTrue(tarEntry.isLink());
  }

  /**
   * Test {@link TarEntry#isLink()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isLink()}
   */
  @Test
  public void testIsLink_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isLink());
  }

  /**
   * Test {@link TarEntry#isCharacterDevice()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code 3}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isCharacterDevice()}
   */
  @Test
  public void testIsCharacterDevice_givenTarEntryWithNameLinkFlagIs3_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) '3');

    // Act and Assert
    assertTrue(tarEntry.isCharacterDevice());
  }

  /**
   * Test {@link TarEntry#isCharacterDevice()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isCharacterDevice()}
   */
  @Test
  public void testIsCharacterDevice_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isCharacterDevice());
  }

  /**
   * Test {@link TarEntry#isBlockDevice()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code 4}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isBlockDevice()}
   */
  @Test
  public void testIsBlockDevice_givenTarEntryWithNameLinkFlagIs4_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) '4');

    // Act and Assert
    assertTrue(tarEntry.isBlockDevice());
  }

  /**
   * Test {@link TarEntry#isBlockDevice()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isBlockDevice()}
   */
  @Test
  public void testIsBlockDevice_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isBlockDevice());
  }

  /**
   * Test {@link TarEntry#isFIFO()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code 6}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isFIFO()}
   */
  @Test
  public void testIsFIFO_givenTarEntryWithNameLinkFlagIs6_thenReturnTrue() {
    // Arrange
    TarEntry tarEntry = new TarEntry("Name");
    tarEntry.setLinkFlag((byte) '6');

    // Act and Assert
    assertTrue(tarEntry.isFIFO());
  }

  /**
   * Test {@link TarEntry#isFIFO()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarEntry#isFIFO()}
   */
  @Test
  public void testIsFIFO_givenTarEntryWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new TarEntry("Name")).isFIFO());
  }

  /**
   * Test {@link TarEntry#getDirectoryEntries()}.
   * <p>
   * Method under test: {@link TarEntry#getDirectoryEntries()}
   */
  @Test
  public void testGetDirectoryEntries() {
    // Arrange, Act and Assert
    assertEquals(0, (new TarEntry("Name")).getDirectoryEntries().length);
  }
}
