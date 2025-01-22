package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Iterator;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.selectors.And;
import org.apache.tools.ant.types.resources.selectors.Majority;
import org.apache.tools.ant.types.resources.selectors.ResourceSelector;
import org.apache.tools.ant.types.resources.selectors.Type;
import org.junit.Test;

public class RestrictDiffblueTest {
  /**
   * Test {@link Restrict#add(ResourceCollection)} with {@code c}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link Restrict} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#add(ResourceCollection)}
   */
  @Test
  public void testAddWithC_givenRestrict_whenNone_thenRestrictSizeIsZero() {
    // Arrange
    Restrict restrict = new Restrict();

    // Act
    restrict.add(Resources.NONE);

    // Assert
    assertEquals(0, restrict.size());
    assertTrue(restrict.isEmpty());
  }

  /**
   * Test {@link Restrict#add(ResourceSelector)} with {@code s}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Restrict} (default constructor) ResourceSelectors Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#add(ResourceSelector)}
   */
  @Test
  public void testAddWithS_whenNull_thenRestrictResourceSelectorsEmpty() {
    // Arrange
    Restrict restrict = new Restrict();

    // Act
    restrict.add((ResourceSelector) null);

    // Assert that nothing has changed
    assertFalse(restrict.getSelectors().hasNext());
    assertTrue(restrict.getResourceSelectors().isEmpty());
  }

  /**
   * Test {@link Restrict#iterator()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#DIR}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#iterator()}
   */
  @Test
  public void testIterator_givenRestrictAddDir_thenReturnFailFast() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.DIR);
    restrict.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = restrict.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Restrict#iterator()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link FileList#FileList()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#iterator()}
   */
  @Test
  public void testIterator_givenRestrictAddFileList_thenReturnFailFast() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(new FileList());

    // Act
    Iterator<Resource> actualIteratorResult = restrict.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Restrict#iterator()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Majority#Majority()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#iterator()}
   */
  @Test
  public void testIterator_givenRestrictAddMajority_thenReturnFailFast() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(new Majority());
    restrict.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = restrict.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Restrict#iterator()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#iterator()}
   */
  @Test
  public void testIterator_givenRestrictAddNone_thenReturnFailFast() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = restrict.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenFileNameNameIsName_thenReturnOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act and Assert
    assertEquals(1, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link And#And()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddAnd_thenReturnOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(new And());
    restrict.add(c);

    // Act and Assert
    assertEquals(1, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddAny_thenReturnOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.ANY);
    restrict.add(c);

    // Act and Assert
    assertEquals(1, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddAny_thenReturnOne2() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.ANY);
    restrict.add(Type.ANY);
    restrict.add(c);

    // Act and Assert
    assertEquals(1, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#DIR}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddDir_thenReturnZero() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.DIR);
    restrict.add(c);

    // Act and Assert
    assertEquals(0, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link FileList#FileList()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddFileList_thenReturnZero() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(new FileList());

    // Act and Assert
    assertEquals(0, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Majority#Majority()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddMajority_thenReturnZero() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(new Majority());
    restrict.add(c);

    // Act and Assert
    assertEquals(0, restrict.size());
  }

  /**
   * Test {@link Restrict#size()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#size()}
   */
  @Test
  public void testSize_givenRestrictAddNone_thenReturnZero() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, restrict.size());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatAddFilelistFileList_thenReturnFalse() {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatAddTextText_thenReturnFalse() {
    // Arrange
    Concat c = new Concat();
    c.addText("Text");

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsDot_thenReturnFalse() {
    // Arrange
    FileName name = new FileName();
    name.setName(".");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsName_thenReturnFalse() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsUsers_thenReturnFalse() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link And#And()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrictAddAnd_thenReturnFalse() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(new And());
    restrict.add(new BZip2Resource());

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrictAddAny_thenReturnFalse() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(Type.ANY);
    restrict.add(new BZip2Resource());

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#ANY}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrictAddAny_thenReturnFalse2() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(Type.ANY);
    restrict.add(Type.ANY);
    restrict.add(new BZip2Resource());

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrictAddArchives_thenReturnTrue() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(new Archives());

    // Act and Assert
    assertTrue(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrictAddBZip2Resource_thenReturnFalse() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(new BZip2Resource());

    // Act and Assert
    assertFalse(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrictAddNone_thenReturnTrue() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(Resources.NONE);

    // Act and Assert
    assertTrue(restrict.isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenRestrict_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Restrict()).isFilesystemOnly());
  }

  /**
   * Test {@link Restrict#toString()}.
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList c = new FileList();
    c.addConfiguredFile(name2);
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act
    String actualToStringResult = restrict.toString();

    // Assert
    String toStringResult = Paths.get(System.getProperty("user.dir"), "Users").toString();
    assertEquals(String.join("", toStringResult, ":", Paths.get(System.getProperty("user.dir"), "Name").toString()),
        actualToStringResult);
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link And#And()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddAnd() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(new And());
    restrict.add(c);

    // Act
    String actualToStringResult = restrict.toString();

    // Assert
    assertEquals(Paths.get(System.getProperty("user.dir"), "Name").toString(), actualToStringResult);
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#ANY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddAny() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.ANY);
    restrict.add(c);

    // Act
    String actualToStringResult = restrict.toString();

    // Assert
    assertEquals(Paths.get(System.getProperty("user.dir"), "Name").toString(), actualToStringResult);
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#ANY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddAny2() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.ANY);
    restrict.add(Type.ANY);
    restrict.add(c);

    // Act
    String actualToStringResult = restrict.toString();

    // Assert
    assertEquals(Paths.get(System.getProperty("user.dir"), "Name").toString(), actualToStringResult);
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Type#DIR}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddDir_thenReturnEmptyString() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(Type.DIR);
    restrict.add(c);

    // Act and Assert
    assertEquals("", restrict.toString());
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link FileList#FileList()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddFileList_thenReturnEmptyString() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(new FileList());

    // Act and Assert
    assertEquals("", restrict.toString());
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Majority#Majority()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddMajority_thenReturnEmptyString() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(new Majority());
    restrict.add(c);

    // Act and Assert
    assertEquals("", restrict.toString());
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_givenRestrictAddNone_thenReturnEmptyString() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.add(Resources.NONE);

    // Act and Assert
    assertEquals("", restrict.toString());
  }

  /**
   * Test {@link Restrict#toString()}.
   * <ul>
   *   <li>Then return Property is {@code user.dir} is array of {@link String} with {@code Name} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#toString()}
   */
  @Test
  public void testToString_thenReturnPropertyIsUserDirIsArrayOfStringWithNameToString() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList c = new FileList();
    c.addConfiguredFile(name);

    Restrict restrict = new Restrict();
    restrict.add(c);

    // Act
    String actualToStringResult = restrict.toString();

    // Assert
    assertEquals(Paths.get(System.getProperty("user.dir"), "Name").toString(), actualToStringResult);
  }

  /**
   * Test {@link Restrict#isCache()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor) Cache is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isCache()}
   */
  @Test
  public void testIsCache_givenRestrictCacheIsFalse_thenReturnFalse() {
    // Arrange
    Restrict restrict = new Restrict();
    restrict.setCache(false);

    // Act and Assert
    assertFalse(restrict.isCache());
  }

  /**
   * Test {@link Restrict#isCache()}.
   * <ul>
   *   <li>Given {@link Restrict} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Restrict#isCache()}
   */
  @Test
  public void testIsCache_givenRestrict_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Restrict()).isCache());
  }

  /**
   * Test new {@link Restrict} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Restrict}
   */
  @Test
  public void testNewRestrict() {
    // Arrange and Act
    Restrict actualRestrict = new Restrict();

    // Assert
    Location location = actualRestrict.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRestrict.getDescription());
    assertNull(actualRestrict.getProject());
    assertNull(actualRestrict.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualRestrict.getSelectors().hasNext());
    assertFalse(actualRestrict.isReference());
    assertTrue(actualRestrict.getResourceSelectors().isEmpty());
    assertTrue(actualRestrict.isCache());
  }
}
