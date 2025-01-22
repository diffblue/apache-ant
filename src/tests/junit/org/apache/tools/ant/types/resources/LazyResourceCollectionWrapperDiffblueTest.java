package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class LazyResourceCollectionWrapperDiffblueTest {
  /**
   * Test {@link LazyResourceCollectionWrapper#createIterator()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) add {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#createIterator()}
   */
  @Test
  public void testCreateIterator_givenLazyResourceCollectionWrapperAddNone() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.add(Resources.NONE);

    // Act and Assert
    assertFalse(lazyResourceCollectionWrapper.createIterator().hasNext());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#createIterator()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) Cache is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#createIterator()}
   */
  @Test
  public void testCreateIterator_givenLazyResourceCollectionWrapperCacheIsFalse() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setCache(false);
    lazyResourceCollectionWrapper.add(Resources.NONE);

    // Act and Assert
    assertFalse(lazyResourceCollectionWrapper.createIterator().hasNext());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFileset {@code null}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenConcatAddFilesetNull_thenReturnOne() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFileset(null);

    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.add(c);

    // Act and Assert
    assertEquals(1, lazyResourceCollectionWrapper.getSize());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenLazyResourceCollectionWrapperAddNone_thenReturnZero() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, lazyResourceCollectionWrapper.getSize());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenLazyResourceCollectionWrapperAddNone_thenReturnZero2() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setCache(false);
    lazyResourceCollectionWrapper.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, lazyResourceCollectionWrapper.getSize());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) add {@link Resource#Resource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenLazyResourceCollectionWrapperAddResource_thenReturnOne() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.add(new Resource());

    // Act and Assert
    assertEquals(1, lazyResourceCollectionWrapper.getSize());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link LazyResourceCollectionWrapper} (default constructor) add {@link Resource#Resource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenLazyResourceCollectionWrapperAddResource_thenReturnOne2() throws BuildException {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();
    lazyResourceCollectionWrapper.setCache(false);
    lazyResourceCollectionWrapper.add(new Resource());

    // Act and Assert
    assertEquals(1, lazyResourceCollectionWrapper.getSize());
  }

  /**
   * Test {@link LazyResourceCollectionWrapper#filterResource(Resource)}.
   * <p>
   * Method under test: {@link LazyResourceCollectionWrapper#filterResource(Resource)}
   */
  @Test
  public void testFilterResource() {
    // Arrange
    LazyResourceCollectionWrapper lazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();

    // Act and Assert
    assertFalse(lazyResourceCollectionWrapper.filterResource(new Resource()));
  }

  /**
   * Test new {@link LazyResourceCollectionWrapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LazyResourceCollectionWrapper}
   */
  @Test
  public void testNewLazyResourceCollectionWrapper() {
    // Arrange and Act
    LazyResourceCollectionWrapper actualLazyResourceCollectionWrapper = new LazyResourceCollectionWrapper();

    // Assert
    Location location = actualLazyResourceCollectionWrapper.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLazyResourceCollectionWrapper.getDescription());
    assertNull(actualLazyResourceCollectionWrapper.getProject());
    assertNull(actualLazyResourceCollectionWrapper.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualLazyResourceCollectionWrapper.isReference());
    assertTrue(actualLazyResourceCollectionWrapper.isCache());
  }
}
