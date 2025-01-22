package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class BaseResourceCollectionWrapperDiffblueTest {
  /**
   * Test {@link BaseResourceCollectionWrapper#createIterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionWrapper#createIterator()}
   */
  @Test
  public void testCreateIterator_givenAllButFirstAddNone_thenReturnNotHasNext() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertFalse(allButFirst.createIterator().hasNext());
  }

  /**
   * Test {@link BaseResourceCollectionWrapper#createIterator()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionWrapper#createIterator()}
   */
  @Test
  public void testCreateIterator_givenAllButFirstAddNone_thenReturnNotHasNext2() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertFalse(allButFirst.createIterator().hasNext());
  }

  /**
   * Test {@link BaseResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenAllButFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, allButFirst.getSize());
  }

  /**
   * Test {@link BaseResourceCollectionWrapper#getSize()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseResourceCollectionWrapper#getSize()}
   */
  @Test
  public void testGetSize_givenAllButFirstAddNone_thenReturnZero2() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertEquals(0, allButFirst.getSize());
  }
}
