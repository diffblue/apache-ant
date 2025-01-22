package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.junit.Test;

public class BaseResourceCollectionContainerDiffblueTest {
  /**
   * Test {@link BaseResourceCollectionContainer#isCache()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) Cache is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isCache()}
   */
  @Test
  public void testIsCache_givenDifferenceCacheIsFalse_thenReturnFalse() {
    // Arrange
    Difference difference = new Difference();
    difference.setCache(false);

    // Act and Assert
    assertFalse(difference.isCache());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isCache()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isCache()}
   */
  @Test
  public void testIsCache_givenDifference_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Difference()).isCache());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) Project is {@code null}.</li>
   *   <li>Then {@link Difference} (default constructor) ResourceCollections Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenDifferenceProjectIsNull_thenDifferenceResourceCollectionsEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.setProject(null);

    // Act
    difference.add(null);

    // Assert that nothing has changed
    assertTrue(difference.getResourceCollections().isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) ResourceCollections first is {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenDifference_whenNone_thenDifferenceResourceCollectionsFirstIsNone() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    ResourceCollection c = Resources.NONE;

    // Act
    difference.add(c);

    // Assert
    List<ResourceCollection> resourceCollections = difference.getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertSame(c, resourceCollections.get(0));
  }

  /**
   * Test {@link BaseResourceCollectionContainer#add(ResourceCollection)}.
   * <ul>
   *   <li>Then {@link Difference} (default constructor) ResourceCollections first is {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#add(ResourceCollection)}
   */
  @Test
  public void testAdd_thenDifferenceResourceCollectionsFirstIsNone() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.setProject(new Project());
    ResourceCollection c = Resources.NONE;

    // Act
    difference.add(c);

    // Assert
    List<ResourceCollection> resourceCollections = difference.getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertSame(c, resourceCollections.get(0));
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@link AllButFirst} (default constructor).</li>
   *   <li>Then {@link ArrayList#ArrayList()} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenAllButFirst_whenArrayListAddAllButFirst_thenArrayListSizeIsOne() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    Project project = new Project();
    difference.setProject(project);

    ArrayList<ResourceCollection> c = new ArrayList<>();
    AllButFirst allButFirst = new AllButFirst();
    c.add(allButFirst);

    // Act
    difference.addAll(c);

    // Assert
    assertEquals(1, c.size());
    ResourceCollection getResult = c.get(0);
    assertTrue(getResult instanceof AllButFirst);
    List<ResourceCollection> resourceCollections = difference.getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertSame(project, difference.getProject());
    assertSame(project, ((AllButFirst) getResult).getProject());
    assertSame(allButFirst, resourceCollections.get(0));
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Difference} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenDifferenceProjectIsProject_thenDifferenceProjectIsProject() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    Project project = new Project();
    difference.setProject(project);

    ArrayList<ResourceCollection> c = new ArrayList<>();
    c.add(Resources.NONE);

    // Act
    difference.addAll(c);

    // Assert
    assertEquals(1, difference.getResourceCollections().size());
    assertSame(project, difference.getProject());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then {@link Difference} (default constructor) ResourceCollections Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenDifference_whenArrayList_thenDifferenceResourceCollectionsEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();

    // Act
    difference.addAll(new ArrayList<>());

    // Assert that nothing has changed
    assertTrue(difference.getResourceCollections().isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@link Resources#NONE}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) Collection {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenNone_whenArrayListAddNone_thenDifferenceCollectionList() throws BuildException {
    // Arrange
    Difference difference = new Difference();

    ArrayList<ResourceCollection> c = new ArrayList<>();
    c.add(Resources.NONE);
    c.add(Resources.NONE);

    // Act
    difference.addAll(c);

    // Assert
    Collection<Resource> collection = difference.getCollection();
    assertTrue(collection instanceof List);
    assertEquals(0, difference.size());
    assertEquals(2, difference.getResourceCollections().size());
    assertTrue(collection.isEmpty());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@link Resources#NONE}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenNone_whenArrayListAddNone_thenDifferenceProjectIsNull() throws BuildException {
    // Arrange
    Difference difference = new Difference();

    ArrayList<ResourceCollection> c = new ArrayList<>();
    c.add(Resources.NONE);

    // Act
    difference.addAll(c);

    // Assert
    assertNull(difference.getProject());
    assertEquals(1, difference.getResourceCollections().size());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code null}.</li>
   *   <li>Then {@link Difference} (default constructor) ResourceCollections Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenNull_whenArrayListAddNull_thenDifferenceResourceCollectionsEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();

    ArrayList<ResourceCollection> c = new ArrayList<>();
    c.add(null);

    // Act
    difference.addAll(c);

    // Assert that nothing has changed
    assertTrue(difference.getResourceCollections().isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#addAll(Collection)}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#addAll(Collection)}
   */
  @Test
  public void testAddAll_givenSystemBootClasspath_whenArrayListAddSystemBootClasspath() throws BuildException {
    // Arrange
    Difference difference = new Difference();

    ArrayList<ResourceCollection> c = new ArrayList<>();
    c.add(Path.systemBootClasspath);

    // Act
    difference.addAll(c);

    // Assert
    assertNull(difference.getProject());
    assertEquals(1, difference.getResourceCollections().size());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#iterator()}.
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#iterator()}
   */
  @Test
  public void testIterator() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act
    Iterator<Resource> actualIteratorResult = difference.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertEquals(0, difference.size());
    assertFalse(actualIteratorResult.hasNext());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#iterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#iterator()}
   */
  @Test
  public void testIterator_givenAllButFirstAddNone_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = difference.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertEquals(0, difference.size());
    assertFalse(actualIteratorResult.hasNext());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#iterator()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#iterator()}
   */
  @Test
  public void testIterator_givenDifferenceAddFileResource_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(new FileResource());

    // Act
    Iterator<Resource> actualIteratorResult = difference.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertEquals(0, difference.size());
    assertFalse(actualIteratorResult.hasNext());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#iterator()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#iterator()}
   */
  @Test
  public void testIterator_givenDifferenceAddFiles_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Files());
    difference.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = difference.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertEquals(0, difference.size());
    assertFalse(actualIteratorResult.hasNext());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#iterator()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#iterator()}
   */
  @Test
  public void testIterator_givenDifferenceAddNone_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);
    difference.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = difference.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    assertEquals(0, difference.size());
    assertFalse(actualIteratorResult.hasNext());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#iterator()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return next Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#iterator()}
   */
  @Test
  public void testIterator_givenFilesProjectIsProject_thenReturnNextProjectIsProject() throws BuildException {
    // Arrange
    Files c = new Files();
    Project project = new Project();
    c.setProject(project);
    c.appendIncludes(new String[]{"**"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Iterator<Resource> actualIteratorResult = difference.iterator();

    // Assert
    assertTrue(actualIteratorResult instanceof FailFast);
    Resource nextResult = actualIteratorResult.next();
    assertTrue(nextResult instanceof FileResource);
    assertFalse(actualIteratorResult.hasNext());
    assertSame(project, nextResult.getProject());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize2() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act and Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenDifferenceAddFileResource_thenReturnOne() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenDifferenceAddFileResource_thenReturnZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(new FileResource());

    // Act and Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenDifferenceAddFiles_thenReturnZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Files());
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenDifferenceAddNone_thenReturnZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code **}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenFilesAppendIncludesArrayOfStringWithAsteriskAsterisk_thenReturnOne() throws BuildException {
    // Arrange
    Files c = new Files();
    c.appendIncludes(new String[]{"**"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#size()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#size()}
   */
  @Test
  public void testSize_givenFilesProjectIsProject_thenReturnOne() throws BuildException {
    // Arrange
    Files c = new Files();
    c.setProject(new Project());
    c.appendIncludes(new String[]{"**"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenAllButFirstAddNone_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Difference difference = new Difference();
    difference.add(c);
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Difference} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifferenceAddDifference_thenReturnTrue() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Difference());

    // Act and Assert
    assertTrue(difference.isFilesystemOnly());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link Difference} (default constructor) size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifferenceAddFileResource_thenDifferenceSizeIsOne() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifferenceAddFileResource_thenReturnTrue() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());

    // Act and Assert
    assertTrue(difference.isFilesystemOnly());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifferenceAddFiles_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Files());
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifferenceAddNone_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifferenceAddNone_thenReturnTrue() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);

    // Act and Assert
    assertTrue(difference.isFilesystemOnly());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenDifference_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Difference()).isFilesystemOnly());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code windows}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFilesAppendIncludesArrayOfStringWithWindows() throws BuildException {
    // Arrange
    Files c = new Files();
    c.appendIncludes(new String[]{"windows"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Difference} (default constructor) size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFilesProjectIsProject_thenDifferenceSizeIsZero() throws BuildException {
    // Arrange
    Files c = new Files();
    c.setProject(new Project());
    c.appendIncludes(new String[]{"windows"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(new Archives());

    // Act
    boolean actualIsFilesystemOnlyResult = difference.isFilesystemOnly();

    // Assert
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
    assertTrue(actualIsFilesystemOnlyResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#getResourceCollections()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#getResourceCollections()}
   */
  @Test
  public void testGetResourceCollections_givenDifferenceAddNone_thenReturnSizeIsOne() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);

    // Act
    List<ResourceCollection> actualResourceCollections = difference.getResourceCollections();

    // Assert
    assertEquals(1, actualResourceCollections.size());
    assertTrue(actualResourceCollections.get(0).isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#getResourceCollections()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#getResourceCollections()}
   */
  @Test
  public void testGetResourceCollections_givenDifference_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new Difference()).getResourceCollections().isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#clone()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return ResourceCollections size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#clone()}
   */
  @Test
  public void testClone_givenDifferenceAddNone_thenReturnResourceCollectionsSizeIsOne() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);

    // Act
    Object actualCloneResult = difference.clone();

    // Assert
    assertTrue(actualCloneResult instanceof Difference);
    Location location = ((Difference) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Difference) actualCloneResult).getDescription());
    assertNull(((Difference) actualCloneResult).getProject());
    assertNull(((Difference) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    List<ResourceCollection> resourceCollections = ((Difference) actualCloneResult).getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertFalse(((Difference) actualCloneResult).isReference());
    assertTrue(resourceCollections.get(0).isEmpty());
    assertTrue(((Difference) actualCloneResult).isCache());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#clone()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>Then return ResourceCollections Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#clone()}
   */
  @Test
  public void testClone_givenDifference_thenReturnResourceCollectionsEmpty() {
    // Arrange and Act
    Object actualCloneResult = (new Difference()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof Difference);
    Location location = ((Difference) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Difference) actualCloneResult).getDescription());
    assertNull(((Difference) actualCloneResult).getProject());
    assertNull(((Difference) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((Difference) actualCloneResult).isReference());
    assertTrue(((Difference) actualCloneResult).getResourceCollections().isEmpty());
    assertTrue(((Difference) actualCloneResult).isCache());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act and Assert
    assertEquals("", difference.toString());
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenAllButFirstAddNone_thenReturnEmptyString() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals("", difference.toString());
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link Difference} (default constructor) size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenDifferenceAddFileResource_thenDifferenceSizeIsTwo() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act
    String actualToStringResult = difference.toString();

    // Assert
    assertEquals(2, difference.size());
    assertFalse(difference.isEmpty());
    assertEquals(String.join("", "(unbound file resource):",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()), actualToStringResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenDifferenceAddFileResource_thenReturnEmptyString() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(new FileResource());

    // Act and Assert
    assertEquals("", difference.toString());
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code (unbound file resource)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenDifferenceAddFileResource_thenReturnUnboundFileResource() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals("(unbound file resource)", difference.toString());
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenDifferenceAddFiles_thenReturnEmptyString() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Files());
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals("", difference.toString());
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenDifferenceAddNone_thenReturnEmptyString() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);
    difference.add(Resources.NONE);

    // Act and Assert
    assertEquals("", difference.toString());
    assertEquals(0, difference.size());
    assertTrue(difference.isEmpty());
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_givenFilesProjectIsProject_thenReturnPropertyIsUserDir() throws BuildException {
    // Arrange
    Files c = new Files();
    c.setProject(new Project());
    c.appendIncludes(new String[]{"**"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    String actualToStringResult = difference.toString();

    // Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
    assertEquals(System.getProperty("user.dir"), actualToStringResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Then return Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_thenReturnPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToString() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    difference.add(Resources.NONE);

    // Act
    String actualToStringResult = difference.toString();

    // Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualToStringResult);
  }

  /**
   * Test {@link BaseResourceCollectionContainer#toString()}.
   * <ul>
   *   <li>Then return Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionContainer#toString()}
   */
  @Test
  public void testToString_thenReturnPropertyIsUserDir() throws BuildException {
    // Arrange
    Files c = new Files();
    c.appendIncludes(new String[]{"**"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    String actualToStringResult = difference.toString();

    // Assert
    assertEquals(1, difference.size());
    assertFalse(difference.isEmpty());
    assertEquals(System.getProperty("user.dir"), actualToStringResult);
  }
}
