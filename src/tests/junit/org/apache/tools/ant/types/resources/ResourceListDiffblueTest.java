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
import org.apache.tools.ant.types.FilterChain;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class ResourceListDiffblueTest {
  /**
   * Test {@link ResourceList#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor).</li>
   *   <li>Then {@link ResourceList} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenResourceList_thenResourceListReference() throws BuildException {
    // Arrange
    ResourceList resourceList = new ResourceList();
    Reference r = new Reference("42");

    // Act
    resourceList.setRefid(r);

    // Assert
    assertTrue(resourceList.isReference());
    assertSame(r, resourceList.getRefid());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenConcatAddFilelistFileList_thenReturnFailFast() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenConcatBinaryIsTrue_thenReturnFailFast() {
    // Arrange
    Concat rc = new Concat();
    rc.setBinary(true);
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenConcatDestIsResource_thenReturnFailFast() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code array}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenFileNameNameIsArray_thenReturnFailFast() {
    // Arrange
    FileName name = new FileName();
    name.setName("array");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceListAddFilterChainFilterChain_thenReturnFailFast() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.addFilterChain(new FilterChain());
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceListAddNone_thenReturnFailFast() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceListAddNone_thenReturnFailFast2() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(Resources.NONE);
    resourceList.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) Encoding is {@code UTF-8}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceListEncodingIsUtf8_thenReturnFailFast() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.setEncoding("UTF-8");
    resourceList.addFilterChain(new FilterChain());
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) PreserveDuplicates is {@code true}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceListPreserveDuplicatesIsTrue_thenReturnFailFast() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.setPreserveDuplicates(true);
    resourceList.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) PreserveDuplicates is {@code true}.</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceListPreserveDuplicatesIsTrue_thenReturnFailFast2() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.setPreserveDuplicates(true);
    resourceList.addFilterChain(new FilterChain());
    resourceList.add(rc);

    // Act
    Iterator<Resource> actualIteratorResult = resourceList.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#iterator()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor).</li>
   *   <li>Then return {@link FailFast}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#iterator()}
   */
  @Test
  public void testIterator_givenResourceList_thenReturnFailFast() {
    // Arrange and Act
    Iterator<Resource> actualIteratorResult = (new ResourceList()).iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertFalse(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenConcatAddFilelistFileList_thenReturnZero() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenConcatBinaryIsTrue_thenReturnZero() {
    // Arrange
    Concat rc = new Concat();
    rc.setBinary(true);
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenConcatDestIsResource_thenReturnZero() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code array}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenFileNameNameIsArray_thenReturnZero() {
    // Arrange
    FileName name = new FileName();
    name.setName("array");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenFileNameNameIsUsers_thenReturnZero() {
    // Arrange
    FileName name = new FileName();
    name.setName("array");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListAddFilterChainFilterChain_thenReturnZero() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.addFilterChain(new FilterChain());
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListAddNone_thenReturnZero() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListAddNone_thenReturnZero2() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(Resources.NONE);
    resourceList.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) Encoding is {@code UTF-8}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListEncodingIsUtf8_thenReturnZero() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.setEncoding("UTF-8");
    resourceList.addFilterChain(new FilterChain());
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) PreserveDuplicates is {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListPreserveDuplicatesIsTrue_thenReturnZero() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.setPreserveDuplicates(true);
    resourceList.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) PreserveDuplicates is {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListPreserveDuplicatesIsTrue_thenReturnZero2() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.setPreserveDuplicates(true);
    resourceList.addFilterChain(new FilterChain());
    resourceList.add(rc);

    // Act and Assert
    assertEquals(0, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceListProjectIsProject_thenReturnOne() {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    ResourceList resourceList = new ResourceList();
    resourceList.setProject(new Project());
    resourceList.add(rc);

    // Act and Assert
    assertEquals(1, resourceList.size());
  }

  /**
   * Test {@link ResourceList#size()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#size()}
   */
  @Test
  public void testSize_givenResourceList_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ResourceList()).size());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatAddFilelistFileList_thenReturnTrue() {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatAddNone_thenReturnTrue() {
    // Arrange
    Concat rc = new Concat();
    rc.add(Resources.NONE);

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatBinaryIsTrue_thenReturnTrue() {
    // Arrange
    Concat rc = new Concat();
    rc.setBinary(true);
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenConcatDestIsResource_thenReturnTrue() {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code array}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsArray_thenReturnTrue() {
    // Arrange
    FileName name = new FileName();
    name.setName("array");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Users}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileNameNameIsUsers_thenReturnTrue() {
    // Arrange
    FileName name = new FileName();
    name.setName("array");

    FileName name2 = new FileName();
    name2.setName("Users");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourceList resourceList = new ResourceList();
    resourceList.add(rc);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourceListAddArchives_thenReturnTrue() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(new Archives());

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourceListAddFiles_thenReturnTrue() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(new Files());

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourceListAddNone_thenReturnTrue() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(Resources.NONE);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourceListAddNone_thenReturnTrue2() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.add(Resources.NONE);
    resourceList.add(Resources.NONE);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor) PreserveDuplicates is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourceListPreserveDuplicatesIsTrue_thenReturnTrue() {
    // Arrange
    ResourceList resourceList = new ResourceList();
    resourceList.setPreserveDuplicates(true);
    resourceList.add(Resources.NONE);

    // Act and Assert
    assertTrue(resourceList.isFilesystemOnly());
  }

  /**
   * Test {@link ResourceList#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link ResourceList} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceList#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResourceList_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new ResourceList()).isFilesystemOnly());
  }

  /**
   * Test new {@link ResourceList} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ResourceList}
   */
  @Test
  public void testNewResourceList() {
    // Arrange and Act
    ResourceList actualResourceList = new ResourceList();

    // Assert
    Iterator<Resource> iteratorResult = actualResourceList.iterator();
    assertTrue(iteratorResult instanceof FailFast);
    Location location = actualResourceList.getLocation();
    assertNull(location.getFileName());
    assertNull(actualResourceList.getDescription());
    assertNull(actualResourceList.getProject());
    assertNull(actualResourceList.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualResourceList.size());
    assertFalse(iteratorResult.hasNext());
    assertFalse(actualResourceList.isReference());
    assertTrue(actualResourceList.isEmpty());
    assertTrue(actualResourceList.isFilesystemOnly());
  }
}
