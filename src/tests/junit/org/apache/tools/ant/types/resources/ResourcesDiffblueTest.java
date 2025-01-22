package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class ResourcesDiffblueTest {
  /**
   * Test {@link Resources#Resources()}.
   * <p>
   * Method under test: {@link Resources#Resources()}
   */
  @Test
  public void testNewResources() {
    // Arrange and Act
    Resources actualResources = new Resources();

    // Assert
    Location location = actualResources.getLocation();
    assertNull(location.getFileName());
    assertNull(actualResources.getDescription());
    assertNull(actualResources.getProject());
    assertNull(actualResources.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualResources.size());
    assertFalse(actualResources.isReference());
    assertTrue(actualResources.isEmpty());
  }

  /**
   * Test {@link Resources#Resources(Project)}.
   * <p>
   * Method under test: {@link Resources#Resources(Project)}
   */
  @Test
  public void testNewResources2() {
    // Arrange
    Project project = new Project();

    // Act
    Resources actualResources = new Resources(project);

    // Assert
    assertNull(actualResources.getDescription());
    assertNull(actualResources.getRefid());
    assertEquals(0, actualResources.size());
    assertFalse(actualResources.isReference());
    assertTrue(actualResources.isEmpty());
    assertSame(project, actualResources.getProject());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator() {
    // Arrange
    Concat c = new Concat();
    c.addText("At least one resource must be provided, or some text.");

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        nextResult.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", nextResult.getName());
    assertEquals(-1L, nextResult.getSize());
    assertEquals(0L, nextResult.getLastModified());
    assertFalse(actualIteratorResult.hasNext());
    assertFalse(nextResult.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstAddNone_thenReturnFailFast() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenAllButLastAddNone_thenReturnFailFast() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenConcatDestIsResource() {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertEquals("Concat$ConcatResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals(-1L, nextResult.getSize());
    assertEquals(0L, nextResult.getLastModified());
    assertFalse(actualIteratorResult.hasNext());
    assertFalse(nextResult.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenFileNameNameIsName() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertEquals("Concat$ConcatResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals(-1L, nextResult.getSize());
    assertEquals(0L, nextResult.getLastModified());
    assertFalse(actualIteratorResult.hasNext());
    assertFalse(nextResult.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenFileNameNameIsName2() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(list);

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertEquals("Concat$ConcatResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals(-1L, nextResult.getSize());
    assertEquals(0L, nextResult.getLastModified());
    assertFalse(actualIteratorResult.hasNext());
    assertFalse(nextResult.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenFileNameNameIsUsers() {
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

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertEquals("Concat$ConcatResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals(-1L, nextResult.getSize());
    assertEquals(0L, nextResult.getLastModified());
    assertFalse(actualIteratorResult.hasNext());
    assertFalse(nextResult.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Archives} (default constructor).</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenResourcesAddArchives_thenReturnFailFast() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new Archives());

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Files#Files()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenResourcesAddFiles_thenReturnFailFast() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new Files());

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenResourcesAddNone_thenReturnFailFast() {
    // Arrange
    Resources resources = new Resources();
    resources.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenResourcesAddNone_thenReturnFailFast2() {
    // Arrange
    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_givenResources_thenReturnFailFast() {
    // Arrange and Act
    Iterator<Resource> actualIteratorResult = (new Resources()).iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link Resources#iterator()}.
   * <ul>
   *   <li>Then return next toLongString is {@code Concat$ConcatResource "concat ()"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#iterator()}
   */
  @Test
  public void testIterator_thenReturnNextToLongStringIsConcatConcatResourceConcat() {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Resources resources = new Resources();
    resources.add(c);

    // Act
    Iterator<Resource> actualIteratorResult = resources.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertEquals("Concat$ConcatResource \"concat ()\"", nextResult.toLongString());
    assertEquals("concat ()", nextResult.getName());
    assertEquals(-1L, nextResult.getSize());
    assertEquals(0L, nextResult.getLastModified());
    assertFalse(actualIteratorResult.hasNext());
    assertFalse(nextResult.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(0, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenAllButLastAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(0, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenConcatAddFilelistFileList_thenReturnOne() {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() {
    // Arrange
    Concat c = new Concat();
    c.addText("At least one resource must be provided, or some text.");

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenConcatDestIsResource_thenReturnOne() {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenFileNameNameIsName_thenReturnOne() {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat c = new Concat();
    c.addFilelist(list);

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenFileNameNameIsUsers_thenReturnOne() {
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

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code **}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithAsteriskAsterisk_thenReturnOne() {
    // Arrange
    Files c = new Files();
    c.appendIncludes(new String[]{"**"});

    Resources resources = new Resources();
    resources.add(c);

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Archives} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenResourcesAddArchives_thenReturnZero() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new Archives());

    // Act and Assert
    assertEquals(0, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenResourcesAddBZip2Resource_thenReturnOne() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new BZip2Resource());

    // Act and Assert
    assertEquals(1, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Files#Files()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenResourcesAddFiles_thenReturnZero() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new Files());

    // Act and Assert
    assertEquals(0, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenResourcesAddNone_thenReturnZero() {
    // Arrange
    Resources resources = new Resources();
    resources.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} Cache is {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenResourcesCacheIsTrue_thenReturnZero() {
    // Arrange
    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, resources.size());
  }

  /**
   * Test {@link Resources#size()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#size()}
   */
  @Test
  public void testSize_givenResources_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Resources()).size());
  }

  /**
   * Test {@link Resources#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Archives} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourcesAddArchives_thenReturnFalse() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new Archives());

    // Act and Assert
    assertFalse(resources.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourcesAddNone_thenReturnTrue() {
    // Arrange
    Resources resources = new Resources();
    resources.add(Resources.NONE);

    // Act and Assert
    assertTrue(resources.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourcesAddNone_thenReturnTrue2() {
    // Arrange
    Resources resources = new Resources();
    resources.add(Resources.NONE);
    resources.add(Resources.NONE);

    // Act and Assert
    assertTrue(resources.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} Cache is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourcesCacheIsTrue_thenReturnTrue() {
    // Arrange
    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(Resources.NONE);

    // Act and Assert
    assertTrue(resources.isFilesystemOnly());
  }

  /**
   * Test {@link Resources#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResources_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Resources()).isFilesystemOnly());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenAllButFirstAddNone_thenReturnEmptyString() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(c);

    // Act and Assert
    assertEquals("", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenAllButLastAddNone_thenReturnEmptyString() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(c);

    // Act and Assert
    assertEquals("", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@code concat ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenConcatAddFilelistFileList_thenReturnConcat() {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(c);

    // Act and Assert
    assertEquals("concat ()", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code cr}.</li>
   *   <li>Then return {@code concat (cr)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenConcatAddTextCr_thenReturnConcatCr() {
    // Arrange
    Concat c = new Concat();
    c.addText("cr");

    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(c);

    // Act and Assert
    assertEquals("concat (cr)", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Archives} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenResourcesAddArchives_thenReturnEmptyString() {
    // Arrange
    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(new Archives());

    // Act and Assert
    assertEquals("", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link FileList#FileList()}.</li>
   *   <li>Then return {@code FileList}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenResourcesAddFileList_thenReturnFileList() {
    // Arrange
    Resources resources = new Resources();
    resources.add(new FileList());

    // Act and Assert
    assertEquals("FileList", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Files#Files()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenResourcesAddFiles_thenReturnEmptyString() {
    // Arrange
    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(new Files());

    // Act and Assert
    assertEquals("", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenResourcesAddNone_thenReturnEmptyString() {
    // Arrange
    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(Resources.NONE);

    // Act and Assert
    assertEquals("", resources.toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Given {@link Resources#Resources()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_givenResources_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new Resources()).toString());
  }

  /**
   * Test {@link Resources#toString()}.
   * <ul>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resources#toString()}
   */
  @Test
  public void testToString_thenReturnPropertyIsUserDir() {
    // Arrange
    Files c = new Files();
    c.appendIncludes(new String[]{"**"});

    Resources resources = new Resources();
    resources.setCache(true);
    resources.add(c);

    // Act and Assert
    assertEquals(System.getProperty("user.dir"), resources.toString());
  }
}
