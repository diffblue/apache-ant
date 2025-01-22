package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Comparator;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Parameter;
import org.apache.tools.ant.types.selectors.MockComparator;
import org.junit.Test;

public class SortFilterDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SortFilter#SortFilter(Reader)}
   *   <li>{@link SortFilter#setReverse(boolean)}
   *   <li>{@link SortFilter#getComparator()}
   *   <li>{@link SortFilter#isReverse()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    SortFilter actualSortFilter = new SortFilter(new StringReader("foo"));
    actualSortFilter.setReverse(true);
    Comparator<? super String> actualComparator = actualSortFilter.getComparator();
    boolean actualIsReverseResult = actualSortFilter.isReverse();

    // Assert
    assertNull(actualSortFilter.getParameters());
    assertNull(actualComparator);
    assertNull(actualSortFilter.getProject());
    assertFalse(actualSortFilter.getInitialized());
    assertTrue(actualIsReverseResult);
  }

  /**
   * Test {@link SortFilter#SortFilter()}.
   * <p>
   * Method under test: {@link SortFilter#SortFilter()}
   */
  @Test
  public void testNewSortFilter() {
    // Arrange and Act
    SortFilter actualSortFilter = new SortFilter();

    // Assert
    assertNull(actualSortFilter.getParameters());
    assertNull(actualSortFilter.getComparator());
    assertNull(actualSortFilter.getProject());
    assertFalse(actualSortFilter.getInitialized());
    assertFalse(actualSortFilter.isReverse());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Given {@link Parameter} (default constructor) Name is {@code comparator}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_givenParameterNameIsComparator_thenThrowBuildException() throws IOException {
    // Arrange
    Parameter parameter = new Parameter();
    parameter.setName("comparator");
    parameter.setType("Type");
    parameter.setValue("42");

    SortFilter sortFilter = new SortFilter();
    sortFilter.setParameters(parameter);

    // Act and Assert
    assertThrows(BuildException.class, () -> sortFilter.read());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter(Reader)} with in is {@link StringReader#StringReader(String)} Reverse is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_givenSortFilterWithInIsStringReaderReverseIsTrue() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter(new StringReader("foo"));
    sortFilter.setReverse(true);

    // Act and Assert
    assertEquals(102, sortFilter.read());
    assertTrue(sortFilter.getInitialized());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter(Reader)} with in is {@link StringReader#StringReader(String)} skip three.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_givenSortFilterWithInIsStringReaderSkipThree_thenReturnMinusOne()
      throws IOException, IllegalArgumentException {
    // Arrange
    SortFilter sortFilter = new SortFilter(new StringReader("foo"));
    sortFilter.skip(3L);

    // Act and Assert
    assertEquals(-1, sortFilter.read());
    assertTrue(sortFilter.getInitialized());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_givenSortFilterWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, sortFilter.read());
    assertTrue(sortFilter.getInitialized());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter(new StringReader(""));

    // Act and Assert
    assertEquals(-1, sortFilter.read());
    assertTrue(sortFilter.getInitialized());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter(new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, sortFilter.read());
    assertTrue(sortFilter.getInitialized());
  }

  /**
   * Test {@link SortFilter#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter(new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, sortFilter.read());
    assertTrue(sortFilter.getInitialized());
  }

  /**
   * Test {@link SortFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter()} Reverse is {@code true}.</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return Reverse.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenSortFilterReverseIsTrue_whenStringReaderWithFoo_thenReturnReverse() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter();
    sortFilter.setReverse(true);

    // Act
    Reader actualChainResult = sortFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof SortFilter);
    assertEquals("foo", ((SortFilter) actualChainResult).readFully());
    assertNull(((SortFilter) actualChainResult).getParameters());
    assertNull(((SortFilter) actualChainResult).getComparator());
    assertNull(((SortFilter) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((SortFilter) actualChainResult).getInitialized());
    assertTrue(((SortFilter) actualChainResult).isReverse());
  }

  /**
   * Test {@link SortFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter()}.</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return not Reverse.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenSortFilter_whenStringReaderWithFoo_thenReturnNotReverse() throws IOException {
    // Arrange
    SortFilter sortFilter = new SortFilter();

    // Act
    Reader actualChainResult = sortFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof SortFilter);
    assertEquals("foo", ((SortFilter) actualChainResult).readFully());
    assertNull(((SortFilter) actualChainResult).getParameters());
    assertNull(((SortFilter) actualChainResult).getComparator());
    assertNull(((SortFilter) actualChainResult).getProject());
    assertFalse(((SortFilter) actualChainResult).isReverse());
    assertTrue(actualChainResult.ready());
    assertTrue(((SortFilter) actualChainResult).getInitialized());
  }

  /**
   * Test {@link SortFilter#add(Comparator)}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter()} Comparator is {@link MockComparator} (default constructor).</li>
   *   <li>Then {@link SortFilter#SortFilter()} Comparator is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#add(Comparator)}
   */
  @Test
  public void testAdd_givenSortFilterComparatorIsMockComparator_thenSortFilterComparatorIsNull() {
    // Arrange
    SortFilter sortFilter = new SortFilter();
    sortFilter.setComparator(new MockComparator());

    // Act
    sortFilter.add(null);

    // Assert
    assertNull(sortFilter.getComparator());
  }

  /**
   * Test {@link SortFilter#add(Comparator)}.
   * <ul>
   *   <li>Given {@link SortFilter#SortFilter()} Comparator is {@code null}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link SortFilter#SortFilter()} Comparator is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#add(Comparator)}
   */
  @Test
  public void testAdd_givenSortFilterComparatorIsNull_whenNull_thenSortFilterComparatorIsNull() {
    // Arrange
    SortFilter sortFilter = new SortFilter();
    sortFilter.setComparator(null);

    // Act
    sortFilter.add(null);

    // Assert that nothing has changed
    assertNull(sortFilter.getComparator());
  }

  /**
   * Test {@link SortFilter#add(Comparator)}.
   * <ul>
   *   <li>When {@link MockComparator} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SortFilter#add(Comparator)}
   */
  @Test
  public void testAdd_whenMockComparator_thenThrowBuildException() {
    // Arrange
    SortFilter sortFilter = new SortFilter();
    sortFilter.setComparator(new MockComparator());

    // Act and Assert
    assertThrows(BuildException.class, () -> sortFilter.add(new MockComparator()));
  }
}
