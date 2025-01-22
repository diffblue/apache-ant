package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class SizeLimitCollectionDiffblueTest {
  /**
   * Test {@link SizeLimitCollection#setCount(int)}.
   * <p>
   * Method under test: {@link SizeLimitCollection#setCount(int)}
   */
  @Test
  public void testSetCount() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();

    // Act
    allButFirst.setCount(3);

    // Assert
    assertEquals(3, allButFirst.getCount());
    assertEquals(3, allButFirst.getValidCount());
  }

  /**
   * Test {@link SizeLimitCollection#getCount()}.
   * <p>
   * Method under test: {@link SizeLimitCollection#getCount()}
   */
  @Test
  public void testGetCount() {
    // Arrange, Act and Assert
    assertEquals(1, (new AllButFirst()).getCount());
  }

  /**
   * Test {@link SizeLimitCollection#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);
    allButFirst.setCount(0);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link SizeLimitCollection#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Count is minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#size()}
   */
  @Test
  public void testSize_givenAllButFirstCountIsMinusOne_thenThrowBuildException() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);
    allButFirst.setCount(-1);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.size());
  }

  /**
   * Test {@link SizeLimitCollection#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#size()}
   */
  @Test
  public void testSize_givenFirstAddAllButFirst_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    First first = new First();
    first.add(c);

    // Act and Assert
    assertEquals(0, first.size());
  }

  /**
   * Test {@link SizeLimitCollection#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link First} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#size()}
   */
  @Test
  public void testSize_givenFirstAddFirst_thenReturnZero() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    First first = new First();
    first.add(c);

    // Act and Assert
    assertEquals(0, first.size());
  }

  /**
   * Test {@link SizeLimitCollection#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#size()}
   */
  @Test
  public void testSize_givenFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    First first = new First();
    first.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, first.size());
  }

  /**
   * Test {@link SizeLimitCollection#getValidCount()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) Count is minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#getValidCount()}
   */
  @Test
  public void testGetValidCount_givenAllButFirstCountIsMinusOne_thenThrowBuildException() {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.setCount(-1);

    // Act and Assert
    assertThrows(BuildException.class, () -> allButFirst.getValidCount());
  }

  /**
   * Test {@link SizeLimitCollection#getValidCount()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SizeLimitCollection#getValidCount()}
   */
  @Test
  public void testGetValidCount_givenAllButFirst_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new AllButFirst()).getValidCount());
  }
}
